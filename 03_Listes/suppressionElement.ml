(* Suppresion des éléments d'une liste *)

let rec sup l a = match l with
	[] -> []
	|t::q -> if t=a then sup q a
				else t::(sup q a);;
						 
sup [1;2;3;2;1] 2;;