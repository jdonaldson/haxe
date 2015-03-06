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

@: native("_G.table") extern class Table
{
	static function concat(table: Metatable, ? sep : String, ? i : Int, ? j : Int): String;
	static function foreach(table: Metatable, f: Dynamic->Dynamic->Void): Void;
	static function foreachi(table: Metatable, f: Dynamic->Dynamic->Void): Void;
	static function maxn(table: Metatable): Int;
	static function sort(table: Metatable, ? comp : Dynamic->Dynamic->Bool): Void;
	@: overload(function(table: Metatable, pos: Int, value: Dynamic): Void {})
	static function insert(table: Metatable, value: Dynamic): Void;
	static function remove(table: Metatable, ? pos : Int): Dynamic;
}