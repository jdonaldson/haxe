/*
* Copyright (C)2005-2013 Haxe Foundation
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

#if (macro&&haxe_ver>3.199)
// Haxe 3.2 compatibility
import haxe.ds.StringMap;
import haxe.ds.IntMap;
import haxe.ds.HashMap;
import haxe.ds.ObjectMap;
import haxe.ds.WeakMap;
import haxe.ds.EnumValueMap;
import haxe.Constraints.IMap;
@: multiType(K)
abstract Map<K, V>(IMap<K, V> )
{
	public function new();
	public inline function set(key: K, value: V) this.set(key, value);
	@: arrayAccess public inline function get(key: K) return this.get(key);
	public inline function exists(key: K) return this.exists(key);
	public inline function remove(key: K) return this.remove(key);
	public inline function keys(): Iterator<K>
	return this.keys();
	public inline function iterator(): Iterator<V>
	return this.iterator();
	public inline function toString(): String
	return this.toString();
	@: arrayAccess @: noCompletion public inline function arrayWrite(k: K, v: V): V
	{
		this.set(k, v);
		return v;
	}
	@: to static inline function toStringMap<V>(t: IMap<String, V>): StringMap<V>
	return new StringMap<V>();
	@: to static inline function toIntMap<V>(t: IMap<Int, V>): IntMap<V>
	return new IntMap<V>();
	@: to static inline function toEnumValueMapMap<K: EnumValue, V>
	(t: IMap<K, V>): EnumValueMap<K, V>
	return new EnumValueMap<K, V>();
	@: to static inline function toObjectMap < K: { }, V > (t: IMap<K, V>): ObjectMap<K, V>
	return new ObjectMap<K, V>();
	@: from static inline function fromStringMap<V>(map: StringMap<V>): Map< String, V >
	return cast map;
	@: from static inline function fromIntMap<V>(map: IntMap<V>): Map< Int, V >
	return cast map;
	@: from static inline function fromObjectMap < K: { }, V > (map: ObjectMap<K, V>):
	Map<K, V>
	return cast map;
}
@: deprecated
typedef IMap<K, V> = haxe.Constraints.IMap<K, V>;
#end

#if (!macro||haxe_ver<3.199)

interface IMap<K, V>
{
	function exists(k: K): Bool;
	function get(k: K): Null<V>;
	function iterator(): Iterator<V>;
	function keys(): Iterator<K>;
	function remove(k: K): Bool;
	function set(k: K, v: V): Void;
	function toString(): String;
}

abstract Map<K, V>(IMap<K, V>)
{

	public function new() untyped this = {};

	public inline function set(key: K, value: V): V
	{
		var v = value;
		untyped this[key] = v;
		return v;
	}

	public inline function get(key: K): V return untyped this[key];

	@: arrayAccess @: noCompletion public inline function _get(key: K): V return untyped
			this[key];

	@: arrayAccess @: noCompletion public inline function _set(k: K, value: V): V
	{
		var v = value;
		untyped this[k] = v;
		return v;
	}

	public inline function exists(key: K): Bool return untyped this[key] != null;

	public function remove(key: Dynamic): Bool
	{
		var _has: Bool = exists(key);
		untyped this[key] = null;
		return _has;
	}

	public function keys(): Iterator<K>
	{
		var l = 0;
		var a = untyped ___lua___("{}");
		var t: Dynamic = this;

		untyped __lua__('for k,v in pairs(t) do
		a[l] = k;
		l = l + 1;
		end');

		var i = 0;

		var ret: Dynamic = untyped ___lua___("{}");
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
		var a = untyped ___lua___("{}");
		var t: Dynamic = this;

		untyped __lua__('for k,v in pairs(t) do
		a[l] = v;
		l = l + 1;
		end');

		var i = 0;

		var ret: Dynamic = untyped ___lua___("{}");
		ret.next = function(): K {
			i = i + 1;
			return a[i - 1];
		};
		ret.hasNext = function(): Bool {
			return i < l;
		};

		return ret;
	}

	public inline function toString(): String
	{
		return "" + this;
	}
}

#end