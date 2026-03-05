package unit.issues;

class Issue7350 extends Test {
	function returnInTryCatch(x:Int):Int {
		try {
			if (x > 0) {
				return x * 2;
			}
		} catch (e:Dynamic) {
			return -1;
		}
		return 0;
	}

	function tailReturn(x:Int):Int {
		var result = x + 1;
		return result;
	}

	function test() {
		// Return inside try-catch (pcall) must use do...end wrapper
		eq(10, returnInTryCatch(5));
		eq(0, returnInTryCatch(-1));

		// Tail return should not use do...end wrapper
		eq(6, tailReturn(5));
	}
}
