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

@: native("_G.string") extern class LuaString
{
	static function byte(s: String, ? i : Int, ? j : Int): Int;
	static function char(i: Int): String;
	static function dump(func: Void->Void): String;
	static function find(s: String, ? pattern : String, ? init : Int, ? plain : Bool): Int;
	static function format(s: String, e: Dynamic): String;
	static function gmatch(s: String, pattern: String): Dynamic; // to-do?
	static function gsub(s: String, pattern: String, repl: String, n: Int): String;
	static function len(s: String): Int;
	static function lower(s: String): String;
	static function match(s: String, pattern: String, ? index : Int): String;
	static function rep(s: String, n: Int): String;
	static function reverse(s: String): String;
	static function sub(s: String, i: Int, ? j : Int): String;
	static function upper(s: String): String;
}