(* Calcul d'un seuil *)

let seuil a = 
	let entier = ref 0 and puiss = ref 1 in
	while !puis <= a do
		incr ent;
		puiss:=2*!puiss;
	done; !ent;;