(* Eléments vérifiant une condition *)

let verif l cond = match l with
	[] -> []
	|t::q when cond t -> t::(verif q cond)
	|t::q -> verif q cond;;