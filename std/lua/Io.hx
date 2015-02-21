package lua;

@:native("_G.io") @dotpath extern class Io
{
	static function close(?file:File):Void;
	static function flush():Void;
	static function open(filename:String, ?mode:String):File;
	static function popen(prog:Dynamic, ?mode:String):File;
	static function tmpfile():File;
	static function lines(?filename:String):Dynamic; // to-do?
	static function type(obj:Dynamic):String;
	static function input(?file:File):File;
	static function output(?file:File):File;
	static function read(?format:Dynamic):String;
	static function write(d:Dynamic):Void;
}