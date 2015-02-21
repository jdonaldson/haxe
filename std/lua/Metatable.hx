package lua;

typedef Metatable =
{
	?__call:Dynamic,
	?__mode:String,
	?__metatable:Metatable,
	?__index:Dynamic->Dynamic->Dynamic,
	?__newindex:Dynamic->Dynamic->Dynamic->Void,
	?__eq:Dynamic->Dynamic->Bool,
	?__add:Dynamic->Dynamic->Dynamic,
	?__lt:Dynamic->Dynamic->Bool,
	?__le:Dynamic->Dynamic->Bool,
	?__len:Void->Float,
	?__gc:Dynamic->Void,
	?__sub:Dynamic-> Dynamic->Dynamic,
	?__mul:Dynamic->Dynamic->Dynamic,
	?__div:Dynamic->Dynamic->Dynamic,
	?__mod:Dynamic->Dynamic->Dynamic,
	?__pow:Dynamic->Dynamic->Dynamic,
	?__concat:Dynamic->Dynamic->String,
	?__unm:Dynamic->Dynamic,
	?__tostring:Dynamic->String
}