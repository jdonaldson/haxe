/*
 * Copyright (C)2005-2012 Haxe Foundation
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
 */
@: coreApi class Reflect
{

	public inline static function hasField( o : Dynamic, field : String ) : Bool
	{
		if ((o == null) || (field == null)) return false;
		return untyped o[field] != null;
		// untyped __lua__('Object').prototype.hasOwnProperty.call(o, field);
	}

	public static function field( o : Dynamic, field : String ) : Dynamic
	{
		//try return untyped o[field] catch ( e : Dynamic ) return null;
		//if (untyped o[field] == null) trace("wrong field: " + field);
		//return untyped o && (pcall(function()return o[field])) || nil;
		untyped __lua__("local ok, result = pcall(function()return o[field]end)");
		return untyped ok && result || nil;
	}

	public inline static function setField( o : Dynamic, field : String,
											value : Dynamic ) : Void untyped
	{
		o[field] = value;
	}

	public static inline function getProperty( o : Dynamic,
			field : String ) : Dynamic untyped
	{
		var tmp;
		return if ( o == null ) __define_feature__("Reflect.getProperty", null) else if ( o.__properties__ && (tmp = o.__properties__["get_" + field]) ) o[tmp]() else o[field];
	}

	public static inline function setProperty( o : Dynamic, field : String,
			value : Dynamic ) : Void untyped
	{
		/* TODO
		if o then
		local t = "set_" .. f
		if o[t] then o[t](o, v)
		else Reflect.setField(o, f, v) end
		end
		*/
		var tmp;
		if ( o.__properties__ && (tmp = o.__properties__["set_" + field]) )
			o[tmp](value)
			else
				o[field] = __define_feature__("Reflect.setProperty", value);
	}

	public static function callMethod( o : Dynamic, func : haxe.Constraints.Function, args : Array<Dynamic> ) : Dynamic untyped
	{
		/* TODO
		if o and func and args and o[func] then
		local a = args:copy()
		table.insert(a, 0, o)
		return o[func](o, unpack(a))
		end	return nil
		*/
		return func(o, unpack(args));
	}

	public static function fields( o : Dynamic ) : Array<String>
	{
		var a = [];
		if (o != null)
		{
			//untyped
			//{
			//	var hasOwnProperty = __lua__('Object').prototype.hasOwnProperty;
			//	//__lua__("--for( var f in o ) {");
			//	if ( f != "__id__" && f != "hx__closures__" && hasOwnProperty.call(o, f) ) a.push(f);
			//	//__lua__("--}");
			//}
			if (Std.is(o, String)) return a;
			untyped __lua__("for key, value in pairs (o) do a:push(key); end");
		}
		return a;
	}

	public static function isFunction( f : Dynamic ) : Bool
	{
		return untyped __global__(type, f) == "function";
	}

	public static function compare<T>( a : T, b : T ) : Int
	{
		// If a and b are null, the result is 0.
		if (a == null && b == null) return 0;
		// If only one of them is null, the result is unspecified.
		if (a == null || b == null) return null;
		// This function is only defined if a and b are of the same type.
		untyped __lua__("if type(a) ~= type(b) then return nil end");
		// Numeric types: a is less than b
		untyped __lua__("if type(a) == 'number' then
		if a == b then return 0 else return (a < b) and -1 or 1 end end");
		// String: a is lexicographically less than b
		untyped __lua__("if type(a) == 'string' then
		if a == b then return 0 else return (a < b) and -1 or 1 end end");
		// For all other types, the result is 0 if a and b are equal.
		return (a == b) ? 0 : null;
	}

	public static function compareMethods( f1 : Dynamic, f2 : Dynamic ) : Bool
	{
		// If f1 or f2 are not functions, the result is unspecified.
		if ( !isFunction(f1) || !isFunction(f2) ) return false;
		// Otherwise the result is true if f1 and the f2 are physically equal, false otherwise.
		return ( f1 == f2 );
	}

	public static function isObject( v : Dynamic ) : Bool untyped
	{
		if ( v == null ) return false;
		var t = __global__(type, v);
		return (t == "string" || t == "userdata" || (t == "table" && v.__enum__ == null)) || (t == "function" && (lua.Boot.isClass(v) || lua.Boot.isEnum(v)) != null);
	}

	public static function isEnumValue( v : Dynamic ) : Bool
	{
		return v != null && v.__enum__ != null;
	}

	public static function deleteField( o : Dynamic, field : String ) : Bool
	{
		if ( !hasField(o, field) ) return false;
		untyped o[field] = null;
		return true;
	}

	public static function copy<T>( o : T ) : T
	{
		var o2 : Dynamic = {};
		for ( f in Reflect.fields(o) )
			Reflect.setField(o2, f, Reflect.field(o, f));
		return o2;
	}

	@: overload(function( f : Array<Dynamic> -> Void ) : Dynamic {})
	public static function makeVarArgs( f : Array<Dynamic> -> Dynamic ) : Dynamic
	{
		return untyped __lua__("function(...)
		local a = (pack or table.pack)(...)
		local b = {}
		for k, v in ipairs(a) do b[k-1] = v end
		setmetatable(b, Array)
		return f(b)
		end");
	}
}