(* Plus grand élément d'une liste *)

let rec max l = match l with
	[] -> failwith "vide"
	|[a] -> a
	|t::q -> let m = max q in if m > t then m else t;;