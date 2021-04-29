(* Premier rang où se trouve un élément dans une liste *)

let rec rang l a = match l with
	[] -> failwith "vide"
	|t::q when t = a -> 1
	|t::q -> 1 + rang q a;;