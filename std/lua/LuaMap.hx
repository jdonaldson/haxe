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

@: noDoc
class LuaMap<K, V> implements haxe.Constraints.IMap<K, V>
{
	public inline function new() {}

	public inline function set(key: K, value: V) untyped rawset(this, key, value);

	public inline function get(key: K): V return untyped rawget(this, key);

	public inline function exists(key: K): Bool return untyped (rawget(this, key) != null);

	public function remove(key: Dynamic): Bool
	{
		var _has: Bool = exists(key);
		untyped rawset(this, key, null);
		return _has;
	}

	public function keys(): Iterator<K>
	{
		var l = 0;
		var a = untyped __lua__("{}");
		var t: Dynamic = this;

		untyped __lua__('for k,v in pairs(t) do a[l] = k; l = l + 1; end');

		var i = 0;

		var ret: Dynamic = untyped __lua__("{}");
		ret.next = function(): K {
			i = i + 1;
			return a[i - 1];
		};
		ret.hasNext = function(): Bool {
			return i < l;
		};

		return ret;
	}

	public function iterator(): Iterator<V>
	{
		var l = 0;
		var a = untyped __lua__("{}");
		var t: Dynamic = this;

		untyped __lua__('for k,v in pairs(t) do a[l] = v; l = l + 1; end');

		var i = 0;

		var ret: Dynamic = untyped __lua__("{}");
		ret.next = function(): K {
			i = i + 1;
			return a[i - 1];
		};
		ret.hasNext = function(): Bool {
			return i < l;
		};

		return ret;
	}

	public function toString(): String
	{
		var s: String = "{";
		for (i in keys())
		{
			if (s != "{") s += ", ";
			s = s + i + " => " + untyped rawget(this, i);
		}
		return s + "}";
	}
}