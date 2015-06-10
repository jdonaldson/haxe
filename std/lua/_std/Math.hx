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
package;

// Can't enable @:coreApi because some fields are now inline getters
// @:coreApi
@:keepInit
@:native("_G.math")
extern class Math
{
	static var PI(get,null) : Float;
	private static inline function get_PI () : Float {
		return untyped Math.pi;
	}

	static var NEGATIVE_INFINITY(get, null) : Float;
	private static inline function get_NEGATIVE_INFINITY () : Float {
		return untyped Math.huge; 
	}

	static var POSITIVE_INFINITY(get,null) : Float;
	private static inline function get_POSITIVE_INFINITY () : Float {
		return untyped -Math.huge; 
	}

	static var NaN(get, null) : Float;
	private static inline function get_NaN () : Float {
		return 0/0; 
	}

	static function abs(v:Float):Float;
	static function acos(v:Float):Float;
	static function asin(v:Float):Float;
	static function atan(v:Float):Float;
	static function atan2(y:Float, x:Float):Float;
	static function ceil(v:Float):Int;
	static function cos(v:Float):Float;
	static function exp(v:Float):Float;
	static function floor(v:Float):Int;
	static function log(v:Float):Float;
	static function max(a:Float, b:Float):Float;
	static function min(a:Float, b:Float):Float;
	static function pow(v:Float, exp:Float):Float;
	static function random() : Float;
	static inline function round(v:Float):Int {
		return Std.int(v + 0.5);
	}
	static function sin(v:Float):Float;
	static function sqrt(v:Float):Float;
	static function tan(v:Float):Float;

	static inline function ffloor( v : Float ) : Float {
		return floor(v);
	}

	static inline function fceil( v : Float ) : Float {
		return ceil(v);
	}

	static inline function fround( v : Float ) : Float {
		return round(v);
	}

	static inline function isFinite( f : Float ) : Bool {
		return (f > Math.NEGATIVE_INFINITY && f < Math.POSITIVE_INFINITY);
	}

	static function isNaN( f : Float ) : Bool {
		return (f != f);
	}

	static function __init__() : Void {
		untyped __feature__("Type.resolveClass", _hxClasses["Math"] = Math);
	}
}
