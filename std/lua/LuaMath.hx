package lua;

@:native("_G.math") @dotpath extern class LuaMath
{
	static var huge:Float;
	static var pi:Float;
	static function abs(x:Float):Float;
	static function acos(x:Float):Float;
	static function asin(x:Float):Float;
	static function atan(x:Float):Float;
	static function atan2(y:Float, x:Float):Float;
	static function ceil(x:Float):Int;
	static function floor(x:Float):Int;
	static function cos(x:Float):Float;
	static function fmod(x:Float, y:Float):Float;
	static function sin(x:Float):Float;
	static function tan(x:Float):Float;
	static function cosh(x:Float):Float;
	static function sinh(x:Float):Float;
	static function tanh(x:Float):Float;
	static function deg(x:Float):Float;
	static function rad(x:Float):Float;
	static function exp(x:Float):Float;
	static function log(x:Float):Float;
	static function log10(x:Float):Float;
	static function pow(x:Float, p:Float):Float;
	static function min(a:Float, b:Float):Float;
	static function max(a:Float, b:Float):Float;
	static function modf(x:Float):Float;
	static function sqrt(x:Float):Float;
	@:overload(function(min:Float, max:Float):Float {})
	@:overload(function():Float {})
	static function random(n:Float):Float;
	static function randomseed(x:Float):Void;
	static function frexp(x:Float):Float;
	static function ldexp(m:Float, e:Float):Float;
}