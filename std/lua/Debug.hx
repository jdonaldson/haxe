package lua;

@:native("_G.debug") @dotpath extern class Debug
{
	function debug():Void;
	function getfenv(o:Dynamic):Dynamic;
	function gethook(?thread:Thread):Dynamic;
	function getinfo(?thread:Thread, ?func:Dynamic, ?what:String):Metatable;
	function getlocal(?thread:Thread, ?level:Int, ?local:Dynamic):Dynamic;
	function getmetatable(o:Dynamic):Metatable;
	function getregistry():Metatable;
	function getupvalue(func:Dynamic, up:Dynamic):Dynamic;
	function setfenv(object:Dynamic, table:Metatable):Dynamic;
	function sethook(?thread:Thread, ?hook:Dynamic, ?mask:String, ?count:Int):Dynamic;
	function setlocal(?thread:Thread, ?level:Dynamic, ?local:Dynamic, ?value:Dynamic):String;
	function setmetatable(object:Dynamic, table:Metatable):Void;
	function setupvalue(func:Dynamic, up:Dynamic, value:Dynamic):String;
	function traceback(?thread:Thread, ?message:String, ?level:Int):String;
}