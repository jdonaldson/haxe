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

@: coreApi class String
{
	public var length(default, null) : Int;
	public function new(string: String) : Void {};
	public inline function toString() : String return this;
	public inline function toUpperCase() : String return lua.LuaString.upper(this);
	public inline function toLowerCase() : String return lua.LuaString.lower(this);
	public inline function charAt( index : Int ) : String
	return lua.LuaString.sub(this, index + 1, index + 1);
	public inline function substring( startIndex : Int, ? endIndex : Int ) : String
	return lua.LuaString.sub(this, startIndex, endIndex);
	public inline function charCodeAt( index : Int ) : Null<Int>
	return lua.LuaString.byte(lua.LuaString.sub(this, index + 1, index + 1));

	public function indexOf( str : String, ? startIndex : Int ) : Int
	{
		var r = lua.LuaString.find(this, str, startIndex, true);
		return untyped r && (r - 1) || (-1);
	};

	public function lastIndexOf( str : String, ? startIndex : Int ) : Int
	{
		var i = startIndex;
		var r = 0;
		while (i != null)
		{
			i = indexOf(str, i);
			if (i != null) r = i;
		}
		return r - 1;
	};

	public function split( delimiter : String ) : Array<String>
	{
		var t : Dynamic = untyped __lua__("{}"), ll : Int = 0;
		if (length == 1) return [this];
		while (true)
		{
			var l = lua.LuaString.find(this, delimiter, ll, true);
			if (l != null)
			{
				lua.Table.insert(t, lua.LuaString.sub(this, ll, l - 1));
				ll = l + 1;
			}
			else
			{
				lua.Table.insert(t, lua.LuaString.sub(this, ll));
				break ;
			}
		}
		return lua.Lib.setmetatable(t, Array);
	};

	public function substr( pos : Int, ? len : Int ) : String
	return untyped len
		   && lua.LuaString.sub(this, pos + 1, pos + len)
		   || lua.LuaString.sub(this, pos + 1);

	public inline static function fromCharCode( code : Int ) : String
	return lua.LuaString.char(code);
}