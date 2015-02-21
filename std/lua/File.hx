package lua;

extern class File
{
	function close():Void;
	function flush():Void;
	function lines():Dynamic; // to-do?
	function read(?format:Dynamic):String;
	function seek(?whence:String, ?offset:Int):Dynamic;
	function setvbuf(mode:String, ?size:Int):Void;
	function write(d:Dynamic):Void;
}