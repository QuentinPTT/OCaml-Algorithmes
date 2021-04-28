(* Somme des éléments d'une liste *)

let rec somme l = match l with
	[] -> 0
	|t::q -> t + somme q;;
