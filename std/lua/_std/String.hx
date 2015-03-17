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
		//return 0;
		//var i = startIndex;
		//var r = 0;
		//while (i != -1)
		//{
		//	i = indexOf(str, i);
		//	if (i != -1) r = i;
		//}
		//return r - 1;

		untyped	__lua__("
		local i, j
		local k = 0
		repeat
		i = j
		j, k = string.find(self, str, k + 1, true)
		until j == nil");

		return untyped __lua__("(i or 0) - 1");

		//var i = untyped startIndex || 0;
		//var ret = 0;
		//while (i != -1)
		//{
		//	i = indexOf(str, i);
		//	if (i != -1) ret = i;
		//	if (i >= length) break ;
		//}
		//return ret - 1;

		//var i = 0;//untyped startIndex || 0;
		//var ret = 0;
		//while (i > -1)
		//{
		//	i = indexOf(str, i);
		//	if (i != -1) ret = i;
		//}
		//return ret - 1;
	};

	public function split( delimiter : String ) : Array<String>
	{

		if (length <= 1) return [this];
		if (delimiter.length == 0)
		{
			var r = [];
			for (i in 0...length) r.push(lua.LuaString.sub(this, i + 1, i + 1));
			return r;
		}

		untyped __lua__("local t, ll, d, p
		p = self
		t=setmetatable({}, Array)
		ll=0
		d=delimiter
		while true do
		l=string.find(p,d,ll,true)
		if l~=nil then
		t:push(string.sub(p,ll,l-1))
		ll=l+1
		else
		t:push(string.sub(p,ll))
		break
		end
		end");
		return untyped t;

		//return [];
		//var t : Dynamic = untyped __lua__("{}"), ll : Int = 0;
		//if (length == 1) return [this];
		//while (ll < length)
		//{
		//	var l = lua.LuaString.find(this, delimiter, ll, true);
		//	if (l != null)
		//	{
		//		lua.Table.insert(t, lua.LuaString.sub(this, ll, l - 1));
		//		ll = l + 1;
		//	}
		//	else
		//	{
		//		lua.Table.insert(t, lua.LuaString.sub(this, ll));
		//		break ;
		//	}
		//}
		//return lua.Lib.setmetatable(t, Array);
	};

	public function substr( pos : Int, ? len : Int ) : String
	{
		if ( len == 0 ) return "";
		var sl = length;

		if ( len == null ) len = sl;

		if ( pos == null ) pos = 0;
		if ( pos != 0 && len < 0 )
		{
			return "";
		}

		if ( pos < 0 )
		{
			pos = sl + pos;
			if ( pos < 0 ) pos = 0;
		}
		else if ( len < 0 )
		{
			len = sl + len - pos;
		}

		if ( pos + len > sl )
		{
			len = sl - pos;
		}

		if ( pos < 0 || len <= 0 ) return "";
		return (untyped lua.LuaString.sub(this, pos + 1, pos + len));
	}
	/*return untyped len
		   && lua.LuaString.sub(this, pos + 1, pos + len)
		   || lua.LuaString.sub(this, pos + 1);*/

	public inline static function fromCharCode( code : Int ) : String
	return lua.LuaString.char(code);
}