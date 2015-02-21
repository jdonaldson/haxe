package lua;

@: native("_G.os") @dotpath extern class Os
{
	static function clock(): Float;
	@: overload(function(format: String, ? time : Int): DateTime {})
	static function date(format: String, ? time : Int): DateTime;
	static function difftime(t2: Int, t1: Int): Int;
	static function execute( ? command : String): String;
	static function exit( ? code : Int): Void;
	static function getenv(varname: String): String;
	static function remove(filename: String): Dynamic;
	static function rename(oldname: String, newname: String): Dynamic;
	static function setlocale(locale: String, ? category : String): String;
	static function time(t: DateTime): Int;
	static function tmpname(): String;
}