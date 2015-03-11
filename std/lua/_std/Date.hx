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
@: coreApi class Date
{

	public function new(year : Int, month : Int, day : Int, hour : Int, min : Int,
						sec : Int ) : Void
	{
		var now = Date.now();
		if (year == null) untyped __lua__("return now");
		untyped __lua__("self.d =
		{year = year, day = day, month = month, hour = hour, min = min, sec = sec}
		self.buf = os.time(self.d)");
	};

	public function getTime() : Float return untyped self.buf;
	public function getHours() : Int return untyped self.d.hour;
	public function getMinutes() : Int return untyped self.d.min;
	public function getSeconds() : Int return untyped self.d.sec;
	public function getFullYear() : Int return untyped self.d.year;
	public function getMonth() : Int return untyped self.d.month;
	public function getDate() : Int return untyped self.d.day;
	public function getDay() : Int return untyped os.date("%w", getTime());

	public inline function toString() : String
	{
		return untyped HxOverrides.dateStr(this);
	}

	public static function now() : Date
	{
		if (untyped __lua__("false")) new Date(null, null, null, null, null, null);
		untyped __lua__("local self = {d = os.date('*t')}
		self.buf = os.time(self.d)");
		return untyped __lua__("setmetatable(self, Date)");
	}

	public static function fromTime( t : Float ) : Date
	{
		if (untyped __lua__("false")) new Date(null, null, null, null, null, null);
		untyped __lua__("local self = {d = os.date('*t', t)}
		self.buf = os.time(self.d)");
		return untyped __lua__("setmetatable(self, Date)");
	}

	public static inline function fromString( s : String ) : Date
	{
		return untyped HxOverrides.strDate(s);
	}
}
