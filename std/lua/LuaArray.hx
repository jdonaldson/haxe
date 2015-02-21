package lua;

/*
	LuaArray
	Wrap any Lua array to safely index from [0] in Haxe
*/
abstract LuaArray<T>(Dynamic)
{
	public function new() this = cast untyped __lua__("{}");

	@: arrayAccess public inline function getFromOne(k: Int): T
	{
		return this[k + 1];
	}

	@: arrayAccess public inline function arrayWriteFromOne(k: Int, v: T)
	{
		this[k + 1] = v;
	}

	public var length(get, never): Int;

	public inline function get_length(): Int
	{
		return cast untyped __hash__(this);
	}
}