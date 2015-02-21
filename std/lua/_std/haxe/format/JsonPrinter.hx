/*
* Copyright (C)2005-2014 Haxe Foundation
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

package haxe.format;

class JsonPrinter {

	static public function print(o:Dynamic, ?replacer:Dynamic -> Dynamic -> Dynamic, ?space:String) : String {
		var printer = new JsonPrinter(replacer, space);
		printer.write("", o);
		return printer.buf;
	}

	var buf : String;

	function new(replacer:Dynamic -> Dynamic -> Dynamic, space:String) {
		buf = "";
	}

	function write(k:Dynamic, v:Dynamic):Void
	switch( Type.typeof(v) ) {
		case TUnknown:
			add('"???"');
		case TObject:
			objString(v);
		case TInt:
			add(v);
		case TFloat:
			add(v);
		// TODO add(Math.isFinite(v) ? v : 'null');
		case TFunction:
			add('"<fun>"');
		case TClass(c):
			if( c == String )
				quote(v);
			else if( c == Array ) {
				var v : Array<Dynamic> = v;
				add('[');

				var maxIndex = 0;
				var minIndex = 1234567890;

				untyped __lua__("for _k,_v in pairs(v) do
							maxIndex = math.max(maxIndex,_k)
							minIndex = math.min(minIndex,_k)
							end;");

				maxIndex++;
				var last = maxIndex - 1;
				for (i in minIndex...maxIndex)
				{
					if (i > minIndex) add(',');
					write(i, v[i]);
				}
				add(']');
			//} else if( c == haxe.ds.StringMap ) {
			//	var v : haxe.ds.StringMap<Dynamic> = v;
			//	var o = {};
			//	for( k in v.keys() )
			//		Reflect.setField(o,k,v.get(k));
			//	objString(o);
			} else if( c == Date ) {
				var v : Date = v;
				quote(v.toString());
			} else
				objString(v);
		case TEnum(_):
			var i : Dynamic = Type.enumIndex(v);
			add(i);
		case TBool:
			add(v);
		case TNull:
			add('null');
		case _: add('<json>');
	}

	@:extern inline function add(v:String):Void {
		buf += v;
	}

	@:extern inline function objString( v : Dynamic ):Void {
		fieldsString(v,Reflect.fields(v));
	}

	function fieldsString( v : Dynamic, fields : Array<String> ):Void {
		add('{');
		var len = fields.length;
		var last = len - 1;
		for( i in 0...len ) {
			var f = fields[i];
			var value = Reflect.field(v,f);
			if( Reflect.isFunction(value) ) continue;
			if( i > 0 ) add(',');
			quote(f);
			add(':');
			write(f, value);
		}
		add('}');
	}

	function quote( s : String ):Void {
		add('"');
		var i = 0;
		while( true ) {
			var c = StringTools.fastCodeAt(s, i++);
			if(untyped c == null) break;
			switch( c ) {
			case '"'.code: add('\\"');
			case '\\'.code: add('\\\\');
			case '\n'.code: add('\\n');
			case '\r'.code: add('\\r');
			case '\t'.code: add('\\t');
			case 8: add('\\b');
			case 12: add('\\f');
			default:
				add(String.fromCharCode(c));
			}
		}
		add('"');
	}
}