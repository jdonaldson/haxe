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
@: coreApi
class Array<T> implements ArrayAccess<T>
{
	public var length(default, null) : Int;

	public function new() : Void {};

	public function concat( a : Array<T> ) : Array<T>
	{
		var result = untyped __lua__("{}");
		untyped __lua__(
			"for k,v in pairs(self) do
			result[k] = v
			end
			for i=0,#a do
			result[#result+1] = a[i]
			end");
		return lua.Lib.setmetatable(result, Array);
	}

	public function join( sep : String ) : String
	{
		var t = untyped __lua__("{}");
		untyped __lua__(
			'for i=0, #self do
			t[i] = tostring(self[i] or "")
			end');
		return (untyped __lua__('table.concat'))(t, sep, 0);
	}

	public function pop() : Null<T>
	{
		var length = this.length - 1;
		var last = untyped this[length];
		untyped this[length] = null;
		return last;
	}

	public function push(x : T) : Int
	{
		var length = this.length;
		untyped this[length] = x;
		return length;
	}

	public function reverse() : Void untyped __lua__(
		"local length = #self
		if(length < 2) then return end
		for i = 0,length/2,1 do
		local temp = self[i]
		self[i] = self[length-i]
		self[length-i] = temp
		end");

	public function shift() : Null<T>
	{
		var result = this[0];
		var len = length;
		for (i in 0...len) this[i] = this[i + 1];
		this[len - 1] = cast null;
		return result;
	}

	public function slice( pos : Int, ? end : Int ) : Array<T>
	{
		var result = untyped __lua__("{}");
		var ends = end;
		untyped __lua__(
			"for i = pos,(ends or #self)-1 do
			result[i] = self[i]
			end");
		return lua.Lib.setmetatable(result, Array);
	}

	public function sort( f : T -> T -> Int ) : Void
	{
		untyped __lua__(
			"local isSorted = false
			while isSorted == false do
			movedElements = 0
			for x = 0, #self - 1, 1 do
			if f(self[x], self[x + 1]) > 0 then
			movedElements = movedElements + 1
			testedElement = self[x]
			self[x] = self[x + 1]
			self[x + 1] = testedElement
			end
			end
			if movedElements == 0 then
			isSorted = true
			end
			end");
	}

	public function splice( pos : Int, len : Int ) : Array<T>
	{
		/*var result = untyped __lua__("{}");
		untyped __lua__(
			"for i = pos,len do
			result[i] = self[i]
			end
			for i = pos,len-pos do
			self[i] = self[i+pos+1]
			end
			for i = len,self.length do
			self[i] = nil
			end");
		return lua.Lib.setmetatable(result, Array);*/
		var array = this;
		var result = [];
		var removed = [];
		//var argsLen = arguments.length;
		var arrLen = array.length;
		var i : Int;

		// Follow spec more or less
		var start = pos;
		var deleteCount = len;

		// Deal with negative start per spec
		// Don't assume support for Math.min/max
		if (start < 0)
		{
			start = arrLen + start;
			start = (start > 0) ? start : 0;
		}
		else {
			start = (start < arrLen) ? start : arrLen;
		}

		// Deal with deleteCount per spec
		if (deleteCount < 0) deleteCount = 0;

		if (deleteCount > (arrLen - start))
		{
			deleteCount = arrLen - start;
		}

		// Copy members up to start
		for (i in 0...start)
		{
			result[i] = array[i];
		}

		// Add new elements supplied as args
		//for (i = 3...argsLen)
		//{
		//	result.push(arguments[i]);
		//}

		// Copy removed items to removed array
		for (i in start...start + deleteCount)
		{
			removed.push(array[i]);
		}

		// Add those after start + deleteCount
		for (i in start + (deleteCount)...arrLen)
		{
			result.push(array[i]);
		}

		// Update original array

		while (array.length > 0) array.pop();
		//array.length = 0;

		i = result.length;
		while (i-- > 0)
		{
			array[i] = result[i];
		}

		// Return array of removed elements
		return removed;
	}

	public function toString() : String
	{
		if (length == 0) return "[]";
		var s = "[";
		untyped __lua__(
			"local max = -1
			for key, value in pairs (self) do
			max = key > max and key or max
			end
			local first = true
			for i=0,max do
			local value = self[i]
			s = s + (first and value or (\",\" + value))
			first = false
			end");
		return s + "]";
	}

	public function unshift( x : T ) : Void
	{
		var len = length;
		for (i in 0...len) this[len - i] = this[len - i - 1];
		this[0] = x;
	}

	public inline function insert( pos : Int, x : T ) : Void
	{
		var len = length;
		for (i in - 1...len - pos) this[len - i] = this[len - i - 1];
		this[pos] = x;
	}

	public function remove( x : T ) : Bool
	{
		var i = indexOf(x);
		if ( i == -1 ) return false;
		splice(i, 1);
		return true;
		/*var result = indexOf(x);
		if (result == -1)
		{
			return false;
		}
		else {
			var len = length;
			for (i in result...len - 1) this[i] = this[i + 1];
			this[len] = cast null;
			return true;
		}*/
	}

	public function indexOf( x : T, ? fromIndex : Int ) : Int
	{
		/*if (fromIndex == null) fromIndex = 0;
		for (i in fromIndex...length)
			if (x == this[i]) return i;
		return -1;*/
		var i = untyped fromIndex || 0;
		var len = length;
		if (i < 0)
		{
			i += len;
			if (i < 0) i = 0;
		}
		while (i < len)
		{
			if (this[i] == x) return i;
			i++;
		}
		return -1;
	}

	public function lastIndexOf( x : T, ? fromIndex : Int ) : Int
	{
		if (fromIndex == null) fromIndex = length;
		for (i in 0...fromIndex)
			if (x == this[fromIndex - i]) return fromIndex - i;
		return -1;
	}

	public function copy() : Array<T>
	{
		var result = untyped __lua__("{}");
		untyped __lua__(
			"for k,v in pairs(self) do
			result[k] = v
			end");
		return lua.Lib.setmetatable(result, Array);
	}

	public function map<S>(f: T->S): Array<S>
	{
		var result = untyped __lua__("{}");
		untyped __lua__("for k,v in pairs(self) do result[k] = f(v) end");
		return lua.Lib.setmetatable(result, Array);
	}

	public function filter(f: T->Bool): Array<T>
	{
		var result: Array<T> = [];
		for (i in this)
			if (f(i)) result.push(i);
		return result;
	}

	@: runtime public inline function iterator() : Iterator<T>
	{
		var result: Dynamic = untyped __lua__("{}");
		result.cur = 0;
		result.arr = this;
		result.hasNext = function(): Bool {
			return result.cur < result.arr.length;
			//return result.arr[result.cur] != null;
		}
		result.next = function(): T {
			result.cur++;
			return cast result.arr[cast(result.cur - 1)];
		}
		return result;
	}
}
