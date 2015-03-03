package lua;

@: native("_G.string") @dotpath extern class LuaString
{
	static function byte(s: String, i: Int, ? j : Int): Int;
	static function char(i: Int): String;
	static function dump(func: Void->Void): String;
	static function find(s: String, ? pattern : String, ? init : Int, ? plain : Bool): Int;
	static function format(s: String, e: Dynamic): String;
	static function gmatch(s: String, pattern: String): Dynamic; // to-do?
	static function gsub(s: String, pattern: String, repl: String, n: Int): String;
	static function len(s: String): Int;
	static function lower(s: String): String;
	static function match(s: String, pattern: String, ? index : Int): String;
	static function rep(s: String, n: Int): String;
	static function reverse(s: String): String;
	static function sub(s: String, i: Int, ? j : Int): String;
	static function upper(s: String): String;
}