package lua;
/**
  Externs for the "bit" class that is required for haxe lua
**/

@:native("_hx_bit")
extern class Bit {
	public static function bnot(x:Float) : Int;
	public static function band(a:Float, b:Float) : Int;
	public static function bor(a:Float, b:Float) : Int;
	public static function bxor(a:Float, b:Float) : Int;
	public static function lshift(x:Float, places:Int) : Int;
	public static function rshift(x:Float, places:Int) : Int;
	public static function arshift(x:Float, places:Int) : Int;
	public static function mod(numerator:Float, denominator:Float) : Int;
	public static function __init__() : Void {
#if (lua_ver >= 5.3)
		haxe.macro.Compiler.includeFile("lua/_lua/_hx_clamp53.lua");
		haxe.macro.Compiler.includeFile("lua/_lua/bitwise.lua");
#else
		haxe.macro.Compiler.includeFile("lua/_lua/_hx_clamp.lua");
#end
		haxe.macro.Compiler.includeFile("lua/_lua/_hx_bit.lua");
	}
}
