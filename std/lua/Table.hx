package lua;

@:native("_G.table") @dotpath extern class Table
{
	static function concat(table:Metatable, ?sep:String, ?i:Int, ?j:Int):String;
	static function foreach(table:Metatable, f:Dynamic->Dynamic->Void):Void;
	static function foreachi(table:Metatable, f:Dynamic->Dynamic->Void):Void;
	static function maxn(table:Metatable):Int;
	static function sort(table:Metatable, ?comp:Dynamic->Dynamic->Bool):Void;
	@:overload(function(table:Metatable, pos:Int, value:Dynamic):Void {})
	static function insert(table:Metatable, value:Dynamic):Void;
	static function remove(table:Metatable, ?pos:Int):Dynamic;
}