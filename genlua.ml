(*
 * Copyright (C)2005-2015 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast
open Type
open Common

type pos = Ast.pos

type ctx = {
	com : Common.context;
	buf : Buffer.t;
	packages : (string list,unit) Hashtbl.t;
	js_modern : bool;
	js_flatten : bool;
	mutable current : tclass;
	mutable statics : (tclass * string * texpr) list;
	mutable inits : texpr list;
	mutable tabs : string;
	mutable in_value : tvar option;
	mutable in_loop : bool;
	mutable handle_break : bool;
	mutable id_counter : int;
	mutable type_accessor : module_type -> string;
	mutable separator : bool;
	mutable found_expose : bool;
}

type object_store = {
	os_name : string;
	mutable os_fields : object_store list;
}

let get_exposed ctx path meta =
	if not ctx.js_modern then []
	else try
		let (_, args, pos) = Meta.get Meta.Expose meta in
		(match args with
			| [ EConst (String s), _ ] -> [s]
			| [] -> [path]
			| _ -> error "Invalid @:expose parameters" pos)
	with Not_found -> []

let dot_path = Ast.s_type_path

let flat_path (p,s) =
	(* Replace _ with _g in paths to prevent name collisions. *)
	let escape str = String.concat "_g" (ExtString.String.nsplit str "_") in

	match p with
	| [] -> escape s
	| _ -> String.concat "_" (List.map escape p) ^ "_" ^ (escape s)

let s_path ctx = if ctx.js_flatten then flat_path else dot_path

let kwds =
	let h = Hashtbl.create 0 in
	List.iter (fun s -> Hashtbl.add h s ()) [
		"nil"; "end"; "until"; "repeat"; "and"; "not"; "or";
		"elseif"; "local"; "then"; "true"; "this"; "while";
		"return"; "null"; "break"; "continue"; "false"; "else"; "do";
		"for"; "function"; "goto"; "if"; "in"; "super";
	];
	h

(* Identifiers Haxe reserves to make the output cleaner. These can still be used in untyped code (TLocal),
   but are escaped upon declaration. *)
let kwds2 =
	let h = Hashtbl.create 0 in
	List.iter (fun s -> Hashtbl.add h s ()) [
		"_G"; "self"; "bit"; "_r";
	];
	h

let valid_lua_ident s =
	String.length s > 0 && try
		for i = 0 to String.length s - 1 do
			match String.unsafe_get s i with
			| 'a'..'z' | 'A'..'Z' | '$' | '_' -> ()
			| '0'..'9' when i > 0 -> ()
			| _ -> raise Exit
		done;
		true
	with Exit ->
		false

let rec is_string_type t =
	(*(not (Type.is_null t)) &&*)
	match follow t with
	| TInst ({cl_path = ([], "String")}, _) -> true
	| TAnon a ->
	   (match !(a.a_status) with
	   | Statics ({cl_path = ([], "String")}) -> true
	   | _ -> false)
	| TAbstract (a,pl) -> is_string_type (Abstract.get_underlying_type a pl)
	| _ -> false

let field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "[\"" ^ s ^ "\"]" else "." ^ s
let ident s = if Hashtbl.mem kwds s then "_G" ^ s else s
let check_var_declaration v = if Hashtbl.mem kwds2 v.v_name then v.v_name <- "_G" ^ v.v_name

let anon_field s = if Hashtbl.mem kwds s || not (valid_lua_ident s) then "['" ^ s ^ "']" else s
let static_field s =
	match s with
	| s -> field s

let has_feature ctx = Common.has_feature ctx.com
let add_feature ctx = Common.add_feature ctx.com

let spr ctx s =
	ctx.separator <- false;
	Buffer.add_string ctx.buf s

let print ctx =
	ctx.separator <- false;
	Printf.kprintf (fun s -> begin
		Buffer.add_string ctx.buf s
	end)

(* just print *)
let pr ctx =
	Printf.kprintf (fun s -> begin
		Buffer.add_string ctx.buf s
	end)

let unsupported p = error "This expression cannot be compiled to Lua" p

(* new line without ; *)
let line ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '}' | '{' | ':' when not ctx.separator -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx "\n%s" ctx.tabs

let newline ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '}' | '{' | ':' when not ctx.separator -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx "\n%s" ctx.tabs

let newprop ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '{' -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx "\n%s," ctx.tabs

let semicolon ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '}' when not ctx.separator -> ()
	| _ -> spr ctx ";"

let rec concat ctx s f = function
	| [] -> ()
	| [x] -> f x
	| x :: l ->
		f x;
		spr ctx s;
		concat ctx s f l

let fun_block ctx f p =
	let e = List.fold_left (fun e (a,c) ->
		match c with
		| None | Some TNull -> e
		| Some c -> Type.concat (Codegen.set_default ctx.com a c p) e
	) f.tf_expr f.tf_args in
	e

let open_block ctx =
	let oldt = ctx.tabs in
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let rec has_return e =
	match e.eexpr with
	| TBlock [] -> false
	| TBlock el -> has_return (List.hd (List.rev el))
	| TReturn _ -> true
	| _ -> false

let rec iter_switch_break in_switch e =
	match e.eexpr with
	| TFunction _ | TWhile _ | TFor _ -> ()
	| TSwitch _ when not in_switch -> iter_switch_break true e
	| TBreak when in_switch -> raise Exit
	| _ -> iter (iter_switch_break in_switch) e

let handle_break ctx e =
	let old = ctx.in_loop, ctx.handle_break in
	ctx.in_loop <- true;
	try
		iter_switch_break false e;
		ctx.handle_break <- false;
		(fun() ->
			ctx.in_loop <- fst old;
			ctx.handle_break <- snd old;
		)
	with
		Exit ->
			spr ctx "local try, catch = _G.pcall(function ()";
			let b = open_block ctx in
			newline ctx;
			ctx.handle_break <- true;
			(fun() ->
				b();
				ctx.in_loop <- fst old;
				ctx.handle_break <- snd old;
				newline ctx;
				spr ctx "end end)if(try == false and e ~= \"__break__\") then error(e) end";
			)

(* `self` is a Lua kw, `this` reserved for Haxe *)
let this ctx = match ctx.in_value with None -> "self" | Some _ -> "this"

let is_dynamic_iterator ctx e =
	let check x =
		has_feature ctx "HxOverrides.iter" && (match follow x.etype with
			| TInst ({ cl_path = [],"Array" },_)
			| TInst ({ cl_kind = KTypeParameter _}, _)
			| TAnon _
			| TDynamic _
			| TMono _ ->
				true
			| _ -> false
		)
	in
	match e.eexpr with
	| TField (x,f) when field_name f = "iterator" -> check x
	| _ ->
		false

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s -> print ctx "\"%s\"" (Ast.s_escape s)
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "nil"
	| TThis -> spr ctx (this ctx)
	| TSuper -> spr ctx (this ctx) (* TODO assert false*)

let rec gen_call ctx e el in_value =
	match e.eexpr , el with
	| TConst TSuper , params ->
		(match ctx.current.cl_super with
		| None -> error "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			print ctx "%s.super(%s" (ctx.type_accessor (TClassDecl c)) (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);
	| TField ({ eexpr = TConst TSuper },f) , params ->
		(match ctx.current.cl_super with
		| None -> error "Missing api.setCurrentClass" e.epos
		| Some (c,_) ->
			let name = field_name f in
			print ctx "%s%s(%s" (ctx.type_accessor (TClassDecl c)) (field name) (this ctx);
			List.iter (fun p -> print ctx ","; gen_value ctx p) params;
			spr ctx ")";
		);

	| TCall ({eexpr = TField(e,((FInstance _ | FAnon _) as ef)) }, _), el ->
		gen_value ctx e;
		print ctx ":%s(" (field_name ef);
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";

	| TField ({eexpr = TConst _} as e, ((FInstance _ | FAnon _) as ef)), el ->
		spr ctx "(";
		gen_value ctx e;
		print ctx "):%s(" (field_name ef);
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";

	| TField (e, ((FInstance _ | FAnon _) as ef)), el ->
		gen_value ctx e;
		print ctx ":%s(" (field_name ef);
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"

	| TCall (x,_) , el when (match x.eexpr with TLocal { v_name = "__js__" } -> false | _ -> true) ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TLocal { v_name = "__new__" }, { eexpr = TConst (TString cl) } :: params ->
		print ctx "%s.new(" cl;
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__call__" }, { eexpr = TConst (TString cl) } :: params -> (* TODO doc *)
		print ctx "%s(" cl;
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__call__" }, e :: params -> (* TODO doc *)
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__tail__" }, cl :: f :: params -> (* TODO doc *)
		gen_value ctx cl;
		spr ctx ":";
		gen_value ctx f;
		spr ctx "(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__dot__" }, cl :: f :: params -> (* TODO doc *)
		gen_value ctx cl;
		spr ctx ".";
		gen_value ctx f;
		spr ctx "(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__pack__" }, obj :: params -> (* TODO doc *)
		spr ctx "{";
		gen_value ctx obj;
		spr ctx "}";
	| TLocal { v_name = "__global__" }, cl :: params -> (* TODO doc *)
		spr ctx "_G.";
		(match cl.eexpr with
			| TConst (TString s) -> spr ctx s
			| _ -> gen_value ctx cl
		);
		spr ctx "(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__new__" }, e :: params ->
		gen_value ctx e;
		spr ctx ".new(";
		concat ctx "," (gen_value ctx) params;
		spr ctx ")";
	| TLocal { v_name = "__js__" }, [{ eexpr = TConst (TString "this") }] ->
		spr ctx (this ctx)
	| TLocal { v_name = "__lua__" }, [{ eexpr = TConst (TString code) }] ->
		spr ctx (String.concat "\n" (ExtString.String.nsplit code "\r\n"))
	| TLocal { v_name = "__hash__" }, e :: params -> (* TODO doc *)
		spr ctx "(#"; gen_value ctx e;
		List.iter (fun el -> spr ctx " + #"; (gen_value ctx el)) params;
		spr ctx ")"
	| TLocal { v_name = "__js__" }, { eexpr = TConst (TString code); epos = p } :: tl ->
		Codegen.interpolate_code ctx.com code tl (spr ctx) (gen_expr ctx) p
	| TLocal { v_name = "__instanceof__" },  [o;t] ->
		spr ctx "instanceof(";
		gen_value ctx o;
		print ctx " , ";
		gen_value ctx t;
		spr ctx ")";
	| TLocal { v_name = "__typeof__" },  [o] ->
		spr ctx "typeof(";
		gen_value ctx o;
		spr ctx ")";
	| TLocal { v_name = "__strict_eq__" } , [x;y] ->
		(* add extra parenthesis here because of operator precedence *)
		spr ctx "((";
		gen_value ctx x;
		spr ctx ") == ";
		gen_value ctx y;
		spr ctx ")";
	| TLocal { v_name = "__strict_neq__" } , [x;y] ->
		(* add extra parenthesis here because of operator precedence *)
		spr ctx "((";
		gen_value ctx x;
		spr ctx ") ~= ";
		gen_value ctx y;
		spr ctx ")";
	| TLocal ({v_name = "__define_feature__"}), [_;e] ->
		gen_expr ctx e
	| TLocal { v_name = "__feature__" }, { eexpr = TConst (TString f) } :: eif :: eelse ->
		(if has_feature ctx f then
			gen_value ctx eif
		else match eelse with
			| [] -> ()
			| e :: _ -> gen_value ctx e)
	| TLocal { v_name = "__resources__" }, [] ->
		spr ctx "{";
		concat ctx "," (fun (name,data) ->
			spr ctx "{ ";
			spr ctx "name = ";
			gen_constant ctx e.epos (TString name);
			spr ctx ", data = ";
			gen_constant ctx e.epos (TString (Codegen.bytes_serialize data));
			spr ctx "}"
		) (Hashtbl.fold (fun name data acc -> (name,data) :: acc) ctx.com.resources []);
		spr ctx "}";
	| TLocal { v_name = "`trace" }, [e;infos] ->
		if has_feature ctx "haxe.Log.trace" then begin
			let t = (try List.find (fun t -> t_path t = (["haxe"],"Log")) ctx.com.types with _ -> assert false) in
			spr ctx (ctx.type_accessor t);
			spr ctx ".trace(";
			gen_value ctx e;
			spr ctx ",";
			gen_value ctx infos;
			spr ctx ")";
		end else begin
			spr ctx "console.log(";
			gen_value ctx e;
			spr ctx ")";
		end
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"

and gen_expr ctx e =
	match e.eexpr with
	| TConst c -> gen_constant ctx e.epos c
	| TLocal v -> spr ctx (ident v.v_name)
	| TArray (e1,{ eexpr = TConst (TString s) }) when valid_lua_ident s && (match e1.eexpr with TConst (TInt _|TFloat _) -> false | _ -> true) ->
		gen_value ctx e1;
		spr ctx (field s)
	| TArray (e1,e2) ->
		gen_value ctx e1;
		spr ctx "[";
		gen_value ctx e2;
		spr ctx "]";
	| TBinop (op,{ eexpr = TField (x,f) },e2) when field_name f = "iterator" ->
		gen_value ctx x;
		spr ctx (field "iterator");
		print ctx " %s " (Ast.s_binop op);
		gen_value ctx e2;
	| TBinop ((OpXor|OpAnd|OpShl|OpShr|OpUShr|OpOr) as op,e1,e2) ->
		spr ctx (match op with
			| OpXor  ->  "bit.bxor("
			| OpAnd  ->  "bit.band("
			| OpShl  ->  "bit.lshift("
			| OpShr  ->  "bit.rshift("
			| OpUShr ->  "bit.arshift("
			| OpOr   ->  "bit.bor("
			| _ -> "#BINOP"
		);
		gen_value ctx e1;
		spr ctx ",";
		gen_value ctx e2;
		spr ctx ")"
	| TBinop (op,e1,e2) ->
		gen_value ctx e1;
		let o = (match op with
		| OpAssignOp((OpXor|OpAnd|OpShl|OpShr|OpUShr|OpOr) as aop) ->
			spr ctx " = ";
			aop
		| OpAssignOp(aop) ->
			spr ctx " = ";
			gen_value ctx e1;
			aop
		| _ -> op) in
		(match o with
			|OpXor|OpAnd|OpShl|OpShr|OpUShr|OpOr -> gen_value ctx ({ eexpr = TBinop(o, e1, e2); etype = e1.etype; epos = e1.epos})
			| _ -> (match o with
			| OpAdd when (is_string_type e1.etype && is_string_type e2.etype) ->
			(match e1.eexpr, e2.eexpr with
				| TConst TNull, _ -> spr ctx " + "
				| _, TConst TNull -> spr ctx " + "
				| _ -> spr ctx " .. "
			)
			| OpNotEq -> spr ctx " ~= "
			| OpBoolAnd -> spr ctx " and "
			| OpBoolOr -> spr ctx " or "
			| _ -> print ctx " %s " (Ast.s_binop o));
			gen_value ctx e2;
		)
	(*| TField (x,f) when field_name f = "iterator" && is_dynamic_iterator ctx e ->
		add_feature ctx "use.$iterator";
		print ctx "Qiterator(";
		gen_value ctx x;
		print ctx ")";*)
	| TField (x,FClosure (Some ({cl_path=[],"Array"},_), {cf_name="push"})) ->
		(* see https://github.com/HaxeFoundation/haxe/issues/1997 *)
		add_feature ctx "use.$arrayPushClosure";
		print ctx "_G.arrayPushClosure(";
		gen_value ctx x;
		print ctx ")"
	| TField (x,FClosure (_,f)) ->
		add_feature ctx "use.$bind";
		(match x.eexpr with
		| TConst _ | TLocal _ ->
			print ctx "_G.bind(";
			gen_value ctx x;
			print ctx ",";
			gen_value ctx x;
			print ctx "%s)" (field f.cf_name)
		| _ ->
			print ctx "(Q_=";
			gen_value ctx x;
			print ctx ",_G.bind(Q_,Q_%s))" (field f.cf_name))
	| TEnumParameter (x,_,i) ->
		gen_value ctx x;
		print ctx "[%i]" (i + 2)
	| TField ({ eexpr = TConst (TInt _ | TFloat _ | TString _) } as x,f) ->
		gen_expr ctx { e with eexpr = TField(mk (TParenthesis x) x.etype x.epos,f) }
	| TField (x, (FInstance(_,_,f) | FStatic(_,f) | FAnon(f))) when Meta.has Meta.SelfCall f.cf_meta ->
		gen_value ctx x;
	| TField (x,f) ->
		gen_value ctx x;
		let name = field_name f in
		spr ctx (match f with FStatic _ -> static_field name | FEnum _ | FInstance _ | FAnon _ | FDynamic _ | FClosure _ -> field name)
	| TTypeExpr t ->
		spr ctx (ctx.type_accessor t)
	| TParenthesis e ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| TMeta (_,e) ->
		gen_expr ctx e
	| TReturn eo ->
		if ctx.in_value <> None then unsupported e.epos;
		(match eo with
		| None ->
			spr ctx "return"
		| Some e ->
			spr ctx "return ";
			gen_value ctx e);
	| TBreak ->
		if not ctx.in_loop then unsupported e.epos;
		if ctx.handle_break then spr ctx "error(\"__break__\")" else spr ctx "break"
	| TContinue ->
		if not ctx.in_loop then unsupported e.epos;
		spr ctx "goto continue"
	| TBlock el ->
		print ctx "do";
		let bend = open_block ctx in
		(*List.iter (gen_block_element ctx) el;*)
		(* do not print after last return *)
		let return : bool ref = ref false in
		List.iter
		(fun i ->
			if not !return then (match i.eexpr with
				| TConst _ | TLocal _ -> ()
				| TReturn _ -> return := true; gen_block_element ctx i
				| _ -> gen_block_element ctx i
			)
		) el;
		bend();
		newline ctx;
		print ctx "end";
	| TFunction f ->
		let old = ctx.in_value, ctx.in_loop in
		ctx.in_value <- None;
		ctx.in_loop <- false;
		print ctx "function(%s)" (String.concat "," (List.map ident (List.map arg_name f.tf_args)));

		let return : bool ref = ref false in
		let fexpr : texpr = (fun_block ctx f e.epos) in
		(match fexpr.eexpr with
		| TBlock el ->
			let bend = open_block ctx in
			(*List.iter (gen_block_element ctx) el;*)
			(* do not print after last return *)
			List.iter
			(fun i ->
				if not !return then (match i.eexpr with
					| TConst _ | TLocal _ -> ()
					| TReturn _ -> return := true; gen_block_element ctx i
					| _ -> gen_block_element ctx i
				)
			) el;

			bend();
			newline ctx
		| _ -> gen_value ctx fexpr
		);

		print ctx "end";
		ctx.in_value <- fst old;
		ctx.in_loop <- snd old;
		ctx.separator <- true
	| TCall (e,el) ->
		gen_call ctx e el false
	| TArrayDecl el ->
		spr ctx "setmetatable({";
		if List.length el > 0 then spr ctx "[0]=";
		concat ctx "," (gen_value ctx) el;
		spr ctx "},Array)"
	| TThrow e ->
		spr ctx "error(";
		gen_value ctx e;
		spr ctx ")";
	| TVar (v,eo) ->
		spr ctx "local ";
		check_var_declaration v;
		spr ctx (ident v.v_name);
		begin match eo with
			| None -> ()
			| Some e ->
				spr ctx " = ";
				gen_value ctx e
		end
	| TNew ({ cl_path = [],"Array" },_,[]) ->
		print ctx "setmetatable({},Array)"
	| TNew (c,_,el) ->
		(match c.cl_constructor with
		| Some cf when Meta.has Meta.SelfCall cf.cf_meta -> ()
		| _ -> print ctx " ");
		print ctx "%s.new(" (ctx.type_accessor (TClassDecl c));
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if (";
		let econd = (match cond.eexpr with
			| TParenthesis ee -> ee
			| _ -> cond
		) in
		(match econd.eexpr with
			| TParenthesis ee -> gen_value ctx ee
			| TUnop(Not,_,ee) -> spr ctx "not "; gen_value ctx ee
			| _ -> gen_value ctx cond
		);
		spr ctx ") then ";
		(*gen_expr ctx e;*) gen_block_element ctx e;
		(match eelse with
		| None -> ()
		| Some e2 ->
			(match e.eexpr with
			| TObjectDecl _ -> ctx.separator <- false
			| _ -> ());
			(*semicolon ctx;*)
			spr ctx " else ";
			(*gen_expr ctx e2*) gen_block_element ctx e2
		);
		spr ctx " end";
	| TUnop ((Increment|Decrement) as op,_,e) ->
		gen_expr ctx e;
		print ctx " = ";
		gen_expr ctx e;
		(match op with (* TODO as-a-value *)
			| Increment -> print ctx " + 1"
			| _ -> print ctx " - 1"
		)
	| TUnop (Not,_,e) ->
		spr ctx "not ";
		gen_value ctx e
	| TUnop (Neg,_,e) ->
		spr ctx "-";
		gen_value ctx e
	| TUnop (NegBits,_,e) ->
		spr ctx "bit.bnot(";
		gen_value ctx e;
		spr ctx ")"
	| TUnop (op,Ast.Prefix,e) ->
		spr ctx (Ast.s_unop op);
		gen_value ctx e
	| TUnop (op,Ast.Postfix,e) ->
		gen_value ctx e;
		spr ctx (Ast.s_unop op)
	| TWhile (cond,e,Ast.NormalWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "while";
		gen_value ctx cond;
		spr ctx " do ::continue::";
		(match e.eexpr with
		| TBlock el ->
			List.iter (gen_block_element ctx) el;
			newline ctx
		| _ -> gen_expr ctx e;
		);
		handle_break();
		newline ctx;
		(*if ctx.handle_break then spr ctx "end" else spr ctx "--end"*)
		spr ctx "end"
	| TWhile (cond,e,Ast.DoWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "repeat ::continue::";
		gen_expr ctx e;
		semicolon ctx;
		spr ctx " until not";
		gen_value ctx cond;
		handle_break();
	| TObjectDecl fields ->
		spr ctx "({ ";
		concat ctx ", " (fun (f,e) -> print ctx "%s = " (anon_field f); gen_value ctx e) fields;
		spr ctx "})";
		ctx.separator <- true
	| TFor (v,it,e) ->
		check_var_declaration v;
		let handle_break = handle_break ctx e in
		let it = ident (match it.eexpr with
			| TLocal v -> v.v_name
			| _ ->
				let id = ctx.id_counter in
				ctx.id_counter <- ctx.id_counter + 1;
				let name = "Qit" ^ string_of_int id in
				print ctx "local %s = " name;
				gen_value ctx it;
				newline ctx;
				name
		) in
		print ctx "while( %s:hasNext() ) do" it;
		let bend = open_block ctx in
		newline ctx;
		print ctx "local %s = %s:next();" (ident v.v_name) it;
		gen_block_element ctx e;
		bend();
		newline ctx;
		spr ctx "end";
		handle_break();
	| TTry (e,catchs) ->
		spr ctx "local try, catch = _G.pcall(function ()";
		gen_expr ctx e;
		let vname = (match catchs with [(v,_)] -> check_var_declaration v; v.v_name | _ ->
			let id = ctx.id_counter in
			ctx.id_counter <- ctx.id_counter + 1;
			"Qe" ^ string_of_int id
		) in
		newline ctx;
		print ctx "end) if (try == false) then local %s = catch;" vname;
		let bend = open_block ctx in
		let last = ref false in
		let else_block = ref false in
		List.iter (fun (v,e) ->
			if !last then () else
			let t = (match follow v.v_type with
			| TEnum (e,_) -> Some (TEnumDecl e)
			| TInst (c,_) -> Some (TClassDecl c)
			| TAbstract (a,_) -> Some (TAbstractDecl a)
			| TFun _
			| TLazy _
			| TType _
			| TAnon _ ->
				assert false
			| TMono _
			| TDynamic _ ->
				None
			) in
			match t with
			| None ->
				last := true;
				if !else_block then print ctx "";
				if vname <> v.v_name then begin
					newline ctx;
					print ctx "local %s = %s;" v.v_name vname;
				end;
				gen_block_element ctx e;
				if !else_block then begin
					newline ctx;
					print ctx "end";
				end
			| Some t ->
				if not !else_block then newline ctx;
				print ctx "if( %s.__instanceof(%s," (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" })) vname;
				gen_value ctx (mk (TTypeExpr t) (mk_mono()) e.epos);
				spr ctx ") ) then";
				let bend = open_block ctx in
				if vname <> v.v_name then begin
					newline ctx;
					print ctx "local %s = %s;" v.v_name vname;
				end;
				gen_block_element ctx e;
				bend();
				newline ctx;
				spr ctx " else ";
				else_block := true
		) catchs;
		if not !last then print ctx "error(%s) end" vname;
		bend();
		newline ctx;
		spr ctx "end";

	| TSwitch (e,cases,def) when List.length cases = 0 ->
		(match def with
		| None -> ()
		| Some e -> gen_expr ctx e;
		)

	| TSwitch (e,cases,def) ->
		spr ctx "local switch = ";
		gen_value ctx e;
		newline ctx;
		let first : bool ref = ref true in
		List.iter (fun (el,e2) ->
			List.iter (fun e ->
				if not !first then spr ctx "else";
				first := false;
				match e.eexpr with
				| TConst(c) when c = TNull ->
					spr ctx "if(switch == nil)then ";
				| _ ->
					spr ctx "if(switch == ";
					gen_value ctx e;
					spr ctx ")then "
			) el;
			let bend = open_block ctx in
			gen_block_element ctx e2;
			if not (has_return e2) then begin
				newline ctx;
				(*print ctx "break";*)
			end;
			bend();
			newline ctx;
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "else ";
			let bend = open_block ctx in
			gen_block_element ctx e;
			bend();
			newline ctx;
		);
		spr ctx "end"
	| TCast (e,_) -> gen_expr ctx e
	| TCast (e,None) -> gen_expr ctx e
	| TCast (e1,Some t) ->
		print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
		gen_expr ctx e1;
		spr ctx " , ";
		spr ctx (ctx.type_accessor t);
		spr ctx ")"

and gen_block_element ?(after=false) ctx e =
	match e.eexpr with
	| TConst _ | TLocal _ | TField _ -> ()
	| TUnop ((Increment|Decrement),_,_) -> newline ctx; gen_expr ctx e
	| TUnop (_,_,ue) -> gen_block_element ctx ue
	| TParenthesis pe -> gen_block_element ctx pe
	| TBinop (op,e1,e2) ->
		(match op with
			| Ast.OpAssign | Ast.OpAssignOp(_) ->
				if not after then newline ctx;
				gen_expr ctx e;
				spr ctx ";";
				if after then newline ctx
			| _ -> gen_block_element ctx e2; gen_block_element ctx e1
		)
	| TArrayDecl el -> concat ctx " " (gen_block_element ctx) el;
	| TEnumParameter (x,_,i) -> gen_block_element ctx x;
	| TArray (e1,e2) ->
		gen_block_element ctx e1;
		gen_block_element ctx e2;
	| TNew ({ cl_path = [],"Array" },_,[]) -> ()
	| TBlock el ->
		List.iter (gen_block_element ~after ctx) el
	| TCall ({ eexpr = TLocal { v_name = "__feature__" } }, { eexpr = TConst (TString f) } :: eif :: eelse) ->
		if has_feature ctx f then
			gen_block_element ~after ctx eif
		else (match eelse with
			| [] -> ()
			| [e] -> gen_block_element ~after ctx e
			| _ -> assert false)
	| TFunction _ -> ()
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> gen_block_element ~after ctx e) fl
	| TCast (ce,_) -> gen_block_element ctx ce
	| _ ->
		if not after then newline ctx;
		gen_expr ctx e;
		spr ctx ";";
		if after then newline ctx

and gen_value ctx e =
	let assign e =
		mk (TBinop (Ast.OpAssign,
			mk (TLocal (match ctx.in_value with None -> assert false | Some v -> v)) t_dynamic e.epos,
			e
		)) e.etype e.epos
	in
	let value() =
		let old = ctx.in_value, ctx.in_loop in
		let r = alloc_var "_r" t_dynamic in
		ctx.in_value <- Some r;
		ctx.in_loop <- false;
		spr ctx "(function(this) ";
		let b = open_block ctx in
		newline ctx;
		spr ctx "local _r";
		newline ctx;
		(fun() ->
			newline ctx;
			spr ctx "return _r";
			b();
			newline ctx;
			spr ctx "end)";
			ctx.in_value <- fst old;
			ctx.in_loop <- snd old;
			print ctx "(%s)" (this ctx)
		)
	in
	(match e.eexpr with
	| TConst c -> gen_constant ctx e.epos c
	| TLocal v -> spr ctx (ident v.v_name)
	(*| TArray _ -> gen_expr ctx e*)
	| TArray (e1,e2) ->
		gen_value ctx e1;
		spr ctx "[";
		gen_value ctx e2;
		spr ctx "]";
	| TUnop(Not,_,eo) -> spr ctx "not "; gen_value ctx eo
	| TUnop(Neg,_,eo) -> spr ctx "-"; gen_value ctx eo
	| TUnop(_,Postfix,eo) ->
		spr ctx "((function()local _r = "; gen_expr ctx eo; spr ctx ";";
		gen_expr ctx e;
		spr ctx ";return _r;end)())";
	| TUnop(_,_,eo) ->
		spr ctx "((function()";
		gen_expr ctx e;
		spr ctx ";return ";
		gen_expr ctx eo;
		spr ctx ";end)())";
	| TBinop((OpAssignOp(_)|OpAssign),e1,e2) ->
		spr ctx "((function()";
		gen_expr ctx e;
		spr ctx ";return ";
		gen_expr ctx e1;
		spr ctx ";end)())";
	| TBinop _
	| TField _
	| TEnumParameter _
	| TTypeExpr _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TNew _
	| TUnop _
	| TFunction _ ->
		gen_expr ctx e
	| TMeta (_,e1) ->
		gen_value ctx e1
	| TCall (e,el) ->
		gen_call ctx e el true
	| TReturn _
	| TBreak
	| TContinue ->
		unsupported e.epos
	| TCast (e1, None) ->
		gen_value ctx e1
	| TCast (e1, Some t) ->
		print ctx "%s.__cast(" (ctx.type_accessor (TClassDecl { null_class with cl_path = ["lua"],"Boot" }));
		gen_value ctx e1;
		spr ctx " , ";
		spr ctx (ctx.type_accessor t);
		spr ctx ")"
	| TVar _
	| TFor _
	| TWhile _
	| TThrow _ ->
		(* value is discarded anyway *)
		let v = value() in
		gen_expr ctx e;
		v()
	| TBlock [e] ->
		gen_value ctx e
	| TBlock el ->
		(* last expression in a block -> is a result *)
		let v = value() in
		let rec loop = function
			| [] ->
				spr ctx "return nil";
			| [e] ->
				(match e.eexpr with
					(*| TBinop(_,_,_) -> gen_value ctx e*)
					| _ -> gen_expr ctx (assign e)
				)
			| e :: l ->
				gen_expr ctx e;
				newline ctx;
				loop l
		in
		loop el;
		v();
	| TIf (cond,e,eo) ->
		(* remove parenthesis unless it's an operation with higher precedence than ?: *)
		let cond = (match cond.eexpr with
			| TParenthesis { eexpr = TBinop ((Ast.OpAssign | Ast.OpAssignOp _),_,_) | TIf _ } -> cond
			| TParenthesis e -> e
			| _ -> cond
		) in
		spr ctx "(";
		(match e.eexpr with
			(* WARN using anything that can be equal to `false` breaks this logic *)
			| TConst(TInt _ | TFloat _ | TString _ | TThis | TSuper) ->
				spr ctx "(";
				gen_value ctx cond;
				spr ctx ")and(";
				gen_value ctx e;
				spr ctx ")or(";
				(match eo with
				| None -> spr ctx "nil"
				| Some e -> gen_value ctx e);
				spr ctx ")";
			| _ ->
				spr ctx "(function() if(";
				gen_value ctx cond;
				spr ctx ") then return (";
				gen_value ctx e;
				spr ctx ") else return ";
				(match eo with
				| None -> spr ctx "nil"
				| Some e -> gen_value ctx e);
				spr ctx " end end)()";
			| _ -> spr ctx "!#TERNAR";
		);
		spr ctx ")";

		(* TODO
		gen_value ctx cond;
		spr ctx "?";
		gen_value ctx e;
		spr ctx ":";
		(match eo with
		| None -> spr ctx "nil"
		| Some e -> gen_value ctx e); *)
	| TSwitch (cond,cases,def) ->
		let v = value() in
		gen_expr ctx (mk (TSwitch (cond,
			List.map (fun (e1,e2) -> (e1,assign e2)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TTry (b,catchs) ->
		let v = value() in
		let block e = mk (TBlock [e]) e.etype e.epos in
		gen_expr ctx (mk (TTry (block (assign b),
			List.map (fun (v,e) -> v, block (assign e)) catchs
		)) e.etype e.epos);
		v()
	)

let generate_package_create ctx (p,_) =
	let rec loop acc = function
		| [] -> ()
		| p :: l when Hashtbl.mem ctx.packages (p :: acc) -> loop (p :: acc) l
		| p :: l ->
			Hashtbl.add ctx.packages (p :: acc) ();
			(match acc with
			| [] ->
				if ctx.js_modern then
					print ctx "--[[local]] %s = {}" p
				else
					print ctx "--[[local]] %s = %s or {}" p p
			| _ ->
				let p = String.concat "." (List.rev acc) ^ (field p) in
				if ctx.js_modern then
					print ctx "%s = {}" p
				else
					print ctx "if(!%s) %s = {}" p p
			);
			ctx.separator <- true;
			newline ctx;
			loop (p :: acc) l
	in
	match p with
	| [] -> print ctx "--[[local]] "
	| _ -> loop [] p

let check_field_name c f =
	match f.cf_name with
	| "prototype" | "__proto__" | "constructor" ->
		error ("The field name '" ^ f.cf_name ^ "'  is not allowed in Lua") (match f.cf_expr with None -> c.cl_pos | Some e -> e.epos);
	| _ -> ()

let gen_class_static_field ctx c f =
	match f.cf_expr with
	| None | Some { eexpr = TConst TNull } when not (has_feature ctx "Type.getClassFields") ->
		()
	| None when is_extern_field f ->
		()
	| None ->
		print ctx "%s%s = nil" (s_path ctx c.cl_path) (static_field f.cf_name);
		newline ctx
	| Some e ->
		match e.eexpr with
		| TFunction _ ->
			let path = (s_path ctx c.cl_path) ^ (static_field f.cf_name) in
			let dot_path = (dot_path c.cl_path) ^ (static_field f.cf_name) in
			ctx.id_counter <- 0;
			print ctx "%s = " path;
			gen_value ctx e;
			newline ctx;
		| _ ->
			ctx.statics <- (c,f.cf_name,e) :: ctx.statics

let can_gen_class_field ctx = function
	| { cf_expr = (None | Some { eexpr = TConst TNull }) } when not (has_feature ctx "Type.getInstanceFields") ->
		false
	| f ->
		not (is_extern_field f)

let gen_class_field ctx c f p =
	check_field_name c f;
	match f.cf_expr with
	| None ->
		(*newprop ctx;*)
		newline ctx;
		print ctx "%s.%s = " p (anon_field f.cf_name);
		print ctx "nil";
	| Some e ->
		(*newprop ctx;*)
		newline ctx;
		print ctx "function %s:%s" p (anon_field f.cf_name);
		ctx.id_counter <- 0;
		(*gen_value ctx e;*)
		(match e.eexpr with
		| TFunction f ->
			let old = ctx.in_value, ctx.in_loop in
			ctx.in_value <- None;
			ctx.in_loop <- false;
			print ctx "(%s)" (String.concat "," (List.map ident (List.map arg_name f.tf_args)));

			let return : bool ref = ref false in
			let fexpr : texpr = (fun_block ctx f e.epos) in
			(match fexpr.eexpr with
			| TBlock el ->
				let bend = open_block ctx in
				(*List.iter (gen_block_element ctx) el;*)
				(* do not print after last return *)
				List.iter
				(fun i ->
					if not !return then (match i.eexpr with
						| TConst _ | TLocal _ -> ()
						| TReturn _ -> return := true; gen_block_element ctx i
						| _ -> gen_block_element ctx i
					)
				) el;

				bend();
				newline ctx
			| _ -> gen_value ctx fexpr
			);

			print ctx "end";
			ctx.in_value <- fst old;
			ctx.in_loop <- snd old;
			ctx.separator <- true
		| _ -> spr ctx "() end");
		ctx.separator <- false

let generate_class___name__ ctx c =
	if has_feature ctx "lua.Boot.isClass" then begin
		let p = s_path ctx c.cl_path in
		print ctx "%s.__name__ = " p;
		if has_feature ctx "Type.getClassName" then
			print ctx "setmetatable({[0]=%s},Array)" (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst c.cl_path @ [snd c.cl_path])))
		else
			print ctx "true";
		newline ctx;
	end

let generate_class ctx c =
	ctx.current <- c;
	ctx.id_counter <- 0;
	(match c.cl_path with
	| [],"Function" -> error "This class redefine a native one" c.cl_pos
	| _ -> ());
	let p = s_path ctx c.cl_path in
	let hxClasses = has_feature ctx "Type.resolveClass" in
	if ctx.js_flatten then
		print ctx "local "
	else
		generate_package_create ctx c.cl_path;
	if ctx.js_modern || not hxClasses then
		print ctx "%s = " p
	else begin
		(* TODO modern? *)
		print ctx "%s = _G.hxClasses[\"%s\"] = " p (dot_path c.cl_path);
	end;
	(match c.cl_kind with
		| KAbstractImpl _ ->
			(* abstract implementations only contain static members and don't need to have constructor functions *)
			print ctx "{}"; ctx.separator <- true
		| _ ->
			(match c.cl_constructor with
			| Some { cf_expr = Some e } ->
				print ctx "{}";

				(* inheritance *)
				let has_class : bool = has_feature ctx "lua.Boot.getClass" && (c.cl_super <> None || c.cl_ordered_fields <> [] || c.cl_constructor <> None) in
				let has_prototype : bool = c.cl_super <> None || has_class || List.exists (can_gen_class_field ctx) c.cl_ordered_fields in
				if has_prototype then begin
					(match c.cl_super with
					| None -> ()
					| Some (csup,_) ->
						let psup : string = ctx.type_accessor (TClassDecl csup) in
						print ctx "\nextend(%s,%s) " p psup;
						print ctx "%s.__super__ = %s" p psup;
						newline ctx;
					);
				end;

				(match e.eexpr with | TFunction f ->
					(* constructor just creates object and calls super() to init it *)
					print ctx "%s.__index = %s %s.new = " p p p;
					print ctx "function(%s)" (String.concat "," (List.map ident (List.map arg_name f.tf_args)));
					print ctx "\nlocal self = setmetatable({}, %s)" p;
					print ctx "%s.super(self" p;

					if List.length f.tf_args > 0 then
					print ctx ",%s" (String.concat "," (List.map ident (List.map arg_name f.tf_args)));

					print ctx ") return self end";

					(* initializer *)
					print ctx "\n%s.super = " p;
					spr ctx "function(self";
					if List.length f.tf_args > 0 then
					print ctx ",%s" (String.concat "," (List.map ident (List.map arg_name f.tf_args)));
					spr ctx ")";
					let fexpr : texpr = (fun_block ctx f e.epos) in
					(match fexpr.eexpr with
						| TBlock el ->
							let bend = open_block ctx in
							List.iter (gen_block_element ctx) el;
							bend();
							newline ctx
						| _ -> gen_value ctx fexpr
					);
					print ctx "return self end";

				| _ -> ());

			| _ -> (print ctx "{}"); ctx.separator <- true)
	);
	newline ctx;
	if ctx.js_modern && hxClasses then begin
		print ctx "_G.hxClasses[\"%s\"] = %s" (dot_path c.cl_path) p;
		newline ctx;
	end;
	generate_class___name__ ctx c;
	(match c.cl_implements with
	| [] -> ()
	| l ->
		print ctx "%s.__interfaces__ = {%s}" p (String.concat "," (List.map (fun (i,_) -> ctx.type_accessor (TClassDecl i)) l));
		newline ctx;
	);

	let gen_props props =
		String.concat "," (List.map (fun (p,v) -> p ^"=\""^v^"\"") props) in
	let has_property_reflection =
		(has_feature ctx "Reflect.getProperty") || (has_feature ctx "Reflect.setProperty") in

	if has_property_reflection then begin
		(match Codegen.get_properties c.cl_ordered_statics with
		| [] -> ()
		| props ->
			print ctx "%s.__properties__ = {%s}" p (gen_props props);
			newline ctx);
	end;

	List.iter (gen_class_static_field ctx c) c.cl_ordered_statics;

	let has_class = has_feature ctx "lua.Boot.getClass" && (c.cl_super <> None || c.cl_ordered_fields <> [] || c.cl_constructor <> None) in
	let has_prototype = c.cl_super <> None || has_class || List.exists (can_gen_class_field ctx) c.cl_ordered_fields in
	if has_prototype then begin
		(match c.cl_super with
		| None -> print ctx "--[[%s.prototype]]" p;
		| Some (csup,_) ->
			let psup = ctx.type_accessor (TClassDecl csup) in
			print ctx "%s.__super__ = %s" p psup;
			newline ctx;
			print ctx "_G.extend(%s,%s)  --%s.prototype = _G.extend(%s.prototype,{})" p psup p psup;
		);

		(*let bend = open_block ctx in*)
		List.iter (fun f -> if can_gen_class_field ctx f then gen_class_field ctx c f p) c.cl_ordered_fields;
		if has_class then begin
			newline ctx;
			print ctx "%s.__class__ = %s" p p;
		end;

		if has_property_reflection then begin
			let props = Codegen.get_properties c.cl_ordered_fields in
			(match c.cl_super with
			| _ when props = [] -> ()
			| Some (csup,_) when Codegen.has_properties csup ->
				newline ctx;
				let psup = s_path ctx csup.cl_path in
				print ctx "--%s.__properties__ = extend(%s.prototype.__properties__,{%s})" p psup (gen_props props)
			| _ ->
				newline ctx;
				print ctx "%s.__properties__ = {%s}" p (gen_props props));
		end;

		(*bend();*)
		print ctx "\n";
		(match c.cl_super with None -> ctx.separator <- true | _ -> print ctx "");
		newline ctx
	end

let generate_enum ctx e =
	let p = s_path ctx e.e_path in
	let ename = List.map (fun s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)) (fst e.e_path @ [snd e.e_path]) in
	if ctx.js_flatten then
		print ctx "local "
	else
		generate_package_create ctx e.e_path;
	print ctx "%s = " p;

	if has_feature ctx "Type.resolveEnum" then print ctx "\n--hxClasses[\"%s\"] = \n" (dot_path e.e_path);

	print ctx "{";
	if has_feature ctx "js.Boot.isEnum" then print ctx "__super__ = Enum, __ename__ = %s," (if has_feature ctx "Type.getEnumName" then "{" ^ String.concat "," ename ^ "}" else "true");
	let ind0 = if List.length e.e_names > 0 then "[0]=" else "" in
	print ctx " __constructs__ = setmetatable({%s%s},Array) }" ind0 (String.concat "," (List.map (fun s -> Printf.sprintf "\"%s\"" s) e.e_names));

	print ctx ("\n%s.new = Enum.new") p;

	ctx.separator <- true;
	newline ctx;
	List.iter (fun n ->
		let f = PMap.find n e.e_constrs in
		print ctx "%s%s = " p (field f.ef_name);
		(match f.ef_type with
		| TFun (args,_) ->
			let sargs = String.concat "," (List.map (fun (n,_,_) -> ident n) args) in
			print ctx "function(%s) return Enum.new(\"%s\",%d,{[0]=%s}) end" sargs f.ef_name f.ef_index sargs;
			if has_feature ctx "may_print_enum" then spr ctx "";
			ctx.separator <- true;
		| _ ->
			print ctx "setmetatable({[0]=\"%s\",[1]=%d},Enum)" f.ef_name f.ef_index;
			newline ctx;
			if has_feature ctx "may_print_enum" then begin
				print ctx "-- %s%s = __tostring" p (field f.ef_name);
				newline ctx;
			end;
			print ctx "%s%s.__enum__ = %s" p (field f.ef_name) p;
		);
		newline ctx
	) e.e_names;
	if has_feature ctx "Type.allEnums" then begin
		let ctors_without_args = List.filter (fun s ->
			let ef = PMap.find s e.e_constrs in
			match follow ef.ef_type with
				| TFun _ -> false
				| _ -> true
		) e.e_names in
		print ctx "%s.__empty_constructs__ = {%s}" p (String.concat "," (List.map (fun s -> Printf.sprintf "%s.%s" p s) ctors_without_args));
		newline ctx
	end;
	match Codegen.build_metadata ctx.com (TEnumDecl e) with
	| None -> ()
	| Some e ->
		print ctx "%s.__meta__ = " p;
		gen_expr ctx e;
		newline ctx

let generate_static ctx (c,f,e) =
	print ctx "%s%s = " (s_path ctx c.cl_path) (static_field f);
	gen_value ctx e;
	newline ctx

let generate_require ctx c = (* TODO *)
	let _, args, mp = Meta.get Meta.JsRequire c.cl_meta in
	let p = (s_path ctx c.cl_path) in

	if ctx.js_flatten then
		spr ctx "local "
	else
		generate_package_create ctx c.cl_path;

	(match args with
	| [(EConst(String(module_name)),_)] ->
		print ctx "%s = require(\"%s\")" p module_name
	| [(EConst(String(module_name)),_) ; (EConst(String(object_path)),_)] ->
		print ctx "%s = require(\"%s\").%s" p module_name object_path
	| _ ->
		error "Unsupported @:jsRequire format" mp);

	newline ctx

let generate_type ctx = function
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e ->
			ctx.inits <- e :: ctx.inits);
		(* Special case, want to add Math.__name__ only when required, handle here since Math is extern *)
		let p = s_path ctx c.cl_path in
		(*if p = "Math" then generate_class___name__ ctx c;*)
		(* Another special case for Std because we do not want to generate it if it's empty. *)
		if p = "Std" && c.cl_ordered_statics = [] then
			()
		else if not c.cl_extern then
			generate_class ctx c
		else if (Meta.has Meta.JsRequire c.cl_meta) && (Meta.has Meta.ReallyUsed c.cl_meta) then
			generate_require ctx c
		else if not ctx.js_flatten && Meta.has Meta.InitPackage c.cl_meta then
			(match c.cl_path with
			| ([],_) -> ()
			| _ -> generate_package_create ctx c.cl_path)
	| TEnumDecl e when e.e_extern ->
		()
	| TEnumDecl e -> generate_enum ctx e
	| TTypeDecl _ | TAbstractDecl _ -> ()

let set_current_class ctx c = ctx.current <- c

let alloc_ctx com =
	let ctx = {
		com = com;
		buf = Buffer.create 16000;
		packages = Hashtbl.create 0;
		js_modern = not (Common.defined com Define.JsClassic);
		js_flatten = Common.defined com Define.JsFlatten;
		statics = [];
		inits = [];
		current = null_class;
		tabs = "";
		in_value = None;
		in_loop = false;
		handle_break = false;
		id_counter = 0;
		type_accessor = (fun _ -> assert false);
		separator = false;
		found_expose = false;
	} in
	ctx.type_accessor <- (fun t ->
		let p = t_path t in
		match t with
		| TClassDecl ({ cl_extern = true } as c) when not (Meta.has Meta.JsRequire c.cl_meta)
			-> dot_path p
		| TEnumDecl { e_extern = true }
			-> dot_path p
		| _ -> s_path ctx p);
	ctx

let gen_single_expr ctx e expr =
	if expr then gen_expr ctx e else gen_value ctx e;
	let str = Buffer.contents ctx.buf in
	Buffer.reset ctx.buf;
	ctx.id_counter <- 0;
	str

let generate com =
	let t = Common.timer "generate lua" in
	(match com.js_gen with
	| Some g -> g()
	| None ->
	let ctx = alloc_ctx com in

	(* Lua pre-boot *)
	spr ctx
"
pcall(require, 'bit32') pcall(require, 'bit') bit = bit or bit32
print = print or (function()end);
Int = Int or {} Number = Number or {} Float = Float or Number; String = String or {};
Enum = Enum or {} Enum.new = function(tag,index,params) return setmetatable({
	[0]=params[0],[1]=index,[2]=params,tag=tag,index=index,params=params
},Enum) end;
local _ = getmetatable('') function _.__add(a,b) return tostring(Std.string(a) .. Std.string(b)) end
_.__index = function(str, p) if (p == 'length') then return string.len(str) else return String[p] end end
_ = nil;
if unpack == nil then
function unpack (t, i)
	i = i or 1
	if t[i] ~= nil then return t[i], unpack(t, i + 1)
end end end;
";

	if has_feature ctx "Class" || has_feature ctx "Type.getClassName" then add_feature ctx "lua.Boot.isClass";
	if has_feature ctx "Enum" || has_feature ctx "Type.getEnumName" then add_feature ctx "lua.Boot.isEnum";

	add_feature ctx "Std.string";

	let exposed = List.concat (List.map (fun t ->
		match t with
			| TClassDecl c ->
				let path = dot_path c.cl_path in
				let class_exposed = get_exposed ctx path c.cl_meta in
				let static_exposed = List.map (fun f ->
					get_exposed ctx (path ^ static_field f.cf_name) f.cf_meta
				) c.cl_ordered_statics in
				List.concat (class_exposed :: static_exposed)
			| _ -> []
		) com.types) in
	let anyExposed = exposed <> [] in
	let exportMap = ref (PMap.create String.compare) in
	let exposedObject = { os_name = ""; os_fields = [] } in
	let toplevelExposed = ref [] in
	List.iter (fun path -> (
		let parts = ExtString.String.nsplit path "." in
		let rec loop p pre = match p with
			| f :: g :: ls ->
				let path = match pre with "" -> f | pre -> (pre ^ "." ^ f) in
				if not (PMap.exists path !exportMap) then (
					let elts = { os_name = f; os_fields = [] } in
					exportMap := PMap.add path elts !exportMap;
					let cobject = match pre with "" -> exposedObject | pre -> PMap.find pre !exportMap in
					cobject.os_fields <- elts :: cobject.os_fields
				);
				loop (g :: ls) path;
			| f :: [] when pre = "" ->
				toplevelExposed := f :: !toplevelExposed;
			| _ -> ()
		in loop parts "";
	)) exposed;

	(if has_feature ctx "Type.resolveClass" || has_feature ctx "Type.resolveEnum" then
	spr ctx "_G.hxClasses = _G.hxClasses or {}\n");

	(if has_feature ctx "may_print_enum" then
	spr ctx "function Enum.__tostring(e)
	if e.params == nil then return (e.tag or '') .. rawget(e, 0) end
	local i = 0; local s = '('
	while e.params[i] do
		if s ~= '(' then s = s .. ',' end
		s = s + e.params[i]
		i = i + 1
	end return (e.tag or '') .. s .. ')'
end\n");

	(if List.exists (function TClassDecl { cl_extern = false; cl_super = Some _ } -> true | _ -> false) com.types then
	print ctx "function extend(to, base) for k, v in pairs(base) do to[k] = to[k] or v end to.__super__ = base end;\n");

	List.iter (generate_type ctx) com.types;
	let rec chk_features e =
		if is_dynamic_iterator ctx e then add_feature ctx "use.$iterator";
		match e.eexpr with
		| TField (_,FClosure _) ->
			add_feature ctx "use.$bind"
		| _ ->
			Type.iter chk_features e
	in
	List.iter chk_features ctx.inits;
	List.iter (fun (_,_,e) -> chk_features e) ctx.statics;

	if has_feature ctx "use.$iterator" then begin
		add_feature ctx "use.$bind";
		print ctx "--function Qiterator(o) if( instanceof(o, Array) )then return function() end return HxOverrides.iter(o); end; return typeof(o.iterator) == 'function' ? Qbind(o,o.iterator) : o.iterator; end";
		(*print ctx "--function Qiterator(o) if( instanceof(o, Array) )then return function() end return HxOverrides.iter(o); end; return typeof(o.iterator) == 'function' ? Qbind(o,o.iterator) : o.iterator; end";*)
		newline ctx;
	end;
	if has_feature ctx "use.$bind" then begin
		print ctx "local Q_, Qfid = 0";
		newline ctx;
		print ctx "function _G.bind(o,m) if(not m)then return nil end; return function(...) local result = m(o, ...); return result; end end;";
		(*print ctx "--function Qbind(o,m) if( m == nil ) then return nil; end if( m.__id__ == nil ) m.__id__ = Qfid++; local f; if( o.hx__closures__ == nil ) o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if( f == nil ) { f = function(){ return f.method.apply(f.scope, arguments); }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; end";*)
		newline ctx;
	end;
	(if has_feature ctx "use.$arrayPushClosure" then
	print ctx "function _G.arrayPushClosure(a) return function(x) a:push(x); end; end;\n");

	(* Lua class after-fixes *)
	spr ctx
"(Array or {}).__index = function(self, p)
if (p == 'length') then local len = 0
	for k in pairs(self) do
		len = math.max(len, k+1)
	end
	return len else return Array[p] end
end
Std = Std or {} function Std.string(s)
	local t = type(s) if t == 'string' then return s
	elseif s == nil then return 'null'
	elseif t == 'function' then return '<function>'
	elseif t == 'userdata' or t == 'thread' then return t
	elseif t == 'table' and type(s.toString) == 'function' then return s:toString() or 'null'
	elseif Enum and (getmetatable(s) == Enum) then return tostring(s)
	elseif t == 'table' then
		local o = '{ '
		local first = true
		for key, value in pairs (s) do
			o = o .. (first and key or (', ' .. key)) .. ': ';
			if type(value) == 'table' and type(value.toString) == 'function' then o = o .. value:toString() or 'null'
			elseif type(value) == 'table' then o = o .. '<...>'
			else o = o .. Std.string(value) end
			first = false
		end
		return o .. ' }'
	else return tostring(s) end
end
String.cca = String.charCodeAt
";

	List.iter (gen_block_element ~after:true ctx) (List.rev ctx.inits);
	List.iter (generate_static ctx) (List.rev ctx.statics);

	(match com.main with
	| None -> spr ctx "\nif unit and unit.Test and unit.Test.main then unit.Test.main() end;"
	| Some e -> gen_expr ctx e; newline ctx);

	let ch = open_out_bin com.file in
	output_string ch (Buffer.contents ctx.buf);
	close_out ch);
	t()