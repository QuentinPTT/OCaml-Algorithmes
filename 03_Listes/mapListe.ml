(* Fonction agissant sur tous les �l�ments de la liste *)

let map f l = match l with
	[] -> []
	|t::q -> f(t)::(map f q);;