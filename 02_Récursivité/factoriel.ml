(* Calcul de factoriel *)

let rec factoriel x = match x with
	|x when x = 0 -> 1
	|x -> x*factoriel(x-1);;
