/*
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
 */

// Can't enable @:coreApi because some fields are now inline getters
// @:coreApi
@: keepInit
class Math
{
	public static var PI(default, null) : Float = untyped math.pi;

	public static var NEGATIVE_INFINITY(get, null) : Float;
	private static function get_NEGATIVE_INFINITY () : Float
	{
		return untyped __lua__("(-1.0 / 0.0)");
	}

	public static var POSITIVE_INFINITY(get, null) : Float;
	private static function get_POSITIVE_INFINITY () : Float
	{
		return untyped __lua__("(1.0 / 0.0)");
	}

	public static var NaN(get, null) : Float;
	private static function get_NaN () : Float
	{
		return untyped __lua__("(0 / 0)");
	}

	public static inline function abs(v: Float): Float
	return untyped __global__(math.abs, v);

	public static inline function acos(v: Float): Float
	return untyped __global__(math.acos, v);

	public static inline function asin(v: Float): Float
	return untyped __global__(math.asin, v);

	public static inline function atan(v: Float): Float
	return untyped __global__(math.atan, v);

	public static inline function atan2(y: Float, x: Float): Float
	return untyped __global__(math.atan2, y, x);

	public static inline function ceil(v: Float): Int
	return round(untyped __global__(math.ceil, v));

	public static inline function cos(v: Float): Float
	return untyped __global__(math.cos, v);

	public static inline function exp(v: Float): Float
	return untyped __global__(math.exp, v);

	public static inline function floor(v: Float): Int
	return round(untyped __global__(math.floor, v));

	public static inline function log(v: Float): Float
	return untyped __global__(math.log, v);

	public static inline function max(a: Float, b: Float): Float
	return untyped __global__(math.max, a);

	public static inline function min(a: Float, b: Float): Float
	return untyped __global__(math.min, a);

	public static inline function pow(v: Float, exp: Float): Float
	return untyped __global__(math.pow, v, exp);

	public static inline function random() : Float
	return untyped __global__(math.random);

	public static function round(v: Float): Int
	{
		if (v >= 0)
			return untyped __global__(math.floor, v + 0.5)
			else return untyped __global__(math.ceil, v - 0.5);
	}

	public static inline function sin(v: Float): Float
	return untyped __global__(math.sin, v);

	public static inline function sqrt(v: Float): Float
	return untyped __global__(math.sqrt, v);

	public static inline function tan(v: Float): Float
	return untyped __global__(math.tan, v);

	public static inline function ffloor( v : Float ) : Float
	{
		return floor(v);
	}

	public static inline function fceil( v : Float ) : Float
	{
		return ceil(v);
	}

	public static inline function fround( v : Float ) : Float
	{
		return round(v);
	}

	public static inline function isFinite( f : Float ) : Bool
	{
		return return f != NEGATIVE_INFINITY && f != POSITIVE_INFINITY && f != NaN;
	}

	public static function isNaN( f : Float ) : Bool
	{
		return untyped __lua__("_G.tostring(f) == 'nan' or _G.tostring(f) == '-nan'");
	}

	public static function __init__() : Void
	{
		untyped __feature__("Type.resolveClass", _G.hxClasses["Math"] = Math);
	}
}
