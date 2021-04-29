(* Fonction agissant sur tous les éléments de la liste *)

let map f l = match l with
	[] -> []
	|t::q -> f(t)::(map f q);;