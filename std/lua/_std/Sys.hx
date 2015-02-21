/*
 * Copyright (C)2005-2012 Haxe Foundation
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
/**
	This class gives you access to many base functionalities of system platforms. Looks in [sys] sub packages for more system APIs.
**/
package luaxe;

/*
	implementation inside sys.lua
*/

class Sys {

	/**
		Print any value on the standard output.
	**/
	public	static function print( v : Dynamic ) : Void {};

	/**
		Print any value on the standard output, followed by a newline
	**/
	public	static function println( v : Dynamic ) : Void {};

	/**
		Returns all the arguments that were passed by the commandline.
	**/
	public	static function args() : Array<String> return null;

	/**
		Returns the value of the given environment variable.
	**/
	public	static function getEnv( s : String ) : String return null;

	/**
		Set the value of the given environment variable.
	**/
	public	static function putEnv( s : String, v : String ) : Void {};

	/**
		Returns the whole environement variables.
	**/
	public	static function environment() : haxe.ds.StringMap<String> return null;

	/**
		Suspend the current execution for the given time (in seconds).
	**/
	public	static function sleep( seconds : Float ) : Void {};

	/**
		Change the current time locale, which will affect [DateTools.format] date formating.
		Returns true if the locale was successfully changed
	**/
	public	static function setTimeLocale( loc : String ) : Bool return false;

	/**
		Get the current working directory (usually the one in which the program was started)
	**/
	public	static function getCwd() : String return null;

	/**
		Change the current working directory.
	**/
	public	static function setCwd( s : String ) : Void {};

	/**
		Returns the name of the system you are running on. For instance :
			"Windows", "Linux", "BSD" and "Mac" depending on your desktop OS.
	**/
	public	static function systemName() : String return null;

	/**
		Run the given command with the list of arguments. The command output will be printed on the same output as the current process.
		The current process will block until the command terminates and it will return the command result (0 if there was no error).
		Read the [sys.io.Process] api for a more complete way to start background processes.
	**/
	public	static function command( cmd : String, ?args : Array<String> ) : Int return 0;

	/**
		Exit the current process with the given error code.
	**/
	public	static function exit( code : Int ) : Void {};

	/**
		Gives the most precise timestamp value (in seconds).
	**/
	public	static function time() : Float return 0;

	/**
		Gives the most precise timestamp value (in seconds) but only account for the actual time spent running on the CPU for the current thread/process.
	**/
	public	static function cpuTime() : Float return 0;

	/**
		Returns the path to the current executable that we are running.
	**/
	public	static function executablePath() : String return null;

	/**
		Read a single input character from the standard input (without blocking) and returns it. Setting [echo] to true will also display it on the output.
	**/
	public	static function getChar( echo : Bool ) : Int return 0;

	/**
		Returns the process standard input, from which you can read what user enters. Usually it will block until the user send a full input line. See [getChar] for an alternative.
	**/
	public	static function stdin() : haxe.io.Input return null;

	/**
		Returns the process standard output on which you can write.
	**/
	public	static function stdout() : haxe.io.Output return null;

	/**
		Returns the process standard error on which you can write.
	**/
	public	static function stderr() : haxe.io.Output return null;

}