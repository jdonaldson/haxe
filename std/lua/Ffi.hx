package lua;

@:native("_G.ffi")
extern class Ffi {
	public static var C : Dynamic<Dynamic>;
	public static function cdef(expr:String) : Void;
	public static function load(name:String, ?global : Bool) : Dynamic;
	public static function typeof(ct: Dynamic) : Dynamic;

	@:native("cast")
	public static function ccast(ct:Dynamic, init:Dynamic) : Dynamic;
	public static function metatype(ct:Dynamic, metatable: Dynamic) : Dynamic;
	public static function gc(cdata:Dynamic, finalizer : Dynamic) : Void;
	public static function sizeof(ct:Dynamic, ?nelem: Dynamic) : Int;
	public static function alignof(ct:Dynamic) : Int;
	public static function offsetof(ct:Dynamic, field:String) : Int;
	public static function istype(ct:Dynamic, obj:Dynamic) : Bool;

}
