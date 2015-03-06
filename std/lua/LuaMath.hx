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

package lua;

@: native("_G.math") extern class LuaMath
{
	static var huge: Float;
	static var pi: Float;
	static function abs(x: Float): Float;
	static function acos(x: Float): Float;
	static function asin(x: Float): Float;
	static function atan(x: Float): Float;
	static function atan2(y: Float, x: Float): Float;
	static function ceil(x: Float): Int;
	static function floor(x: Float): Int;
	static function cos(x: Float): Float;
	static function fmod(x: Float, y: Float): Float;
	static function sin(x: Float): Float;
	static function tan(x: Float): Float;
	static function cosh(x: Float): Float;
	static function sinh(x: Float): Float;
	static function tanh(x: Float): Float;
	static function deg(x: Float): Float;
	static function rad(x: Float): Float;
	static function exp(x: Float): Float;
	static function log(x: Float): Float;
	static function log10(x: Float): Float;
	static function pow(x: Float, p: Float): Float;
	static function min(a: Float, b: Float): Float;
	static function max(a: Float, b: Float): Float;
	static function modf(x: Float): Float;
	static function sqrt(x: Float): Float;
	@: overload(function(min: Float, max: Float): Float {})
	@: overload(function(): Float {})
	static function random(n: Float): Float;
	static function randomseed(x: Float): Void;
	static function frexp(x: Float): Float;
	static function ldexp(m: Float, e: Float): Float;
}