(* Premier rang o� se trouve un �l�ment dans une liste *)

let rec rang l a = match l with
	[] -> failwith "vide"
	|t::q when t = a -> 1
	|t::q -> 1 + rang q a;;