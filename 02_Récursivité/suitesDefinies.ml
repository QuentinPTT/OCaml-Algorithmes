(* Deux suites définies entre-elles *)

let rec u = function
		|0 -> 1
		|n -> v(n-1)+1
and v = function
		|0 -> 2
		|n -> u(n-1)+3