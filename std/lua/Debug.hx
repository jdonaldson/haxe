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

@: native("_G.debug") extern class Debug
{
	function debug(): Void;
	function getfenv(o: Dynamic): Dynamic;
	function gethook( ? thread : Thread): Dynamic;
	function getinfo( ? thread : Thread, ? func : Dynamic, ? what : String): Metatable;
	function getlocal( ? thread : Thread, ? level : Int, ? local : Dynamic): Dynamic;
	function getmetatable(o: Dynamic): Metatable;
	function getregistry(): Metatable;
	function getupvalue(func: Dynamic, up: Dynamic): Dynamic;
	function setfenv(object: Dynamic, table: Metatable): Dynamic;
	function sethook( ? thread : Thread, ? hook : Dynamic, ? mask : String,
					  ? count : Int): Dynamic;
	function setlocal( ? thread : Thread, ? level : Dynamic, ? local : Dynamic,
					   ? value : Dynamic): String;
	function setmetatable(object: Dynamic, table: Metatable): Void;
	function setupvalue(func: Dynamic, up: Dynamic, value: Dynamic): String;
	function traceback( ? thread : Thread, ? message : String, ? level : Int): String;
}