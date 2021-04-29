(* Exponentiation rapide *)

let rec expo x n = match n with
	0 -> 1
	|n when n mod 2 = 0 -> let a = expo x (n/2) in a*a
	|_ -> x*(expo x (n-1));;