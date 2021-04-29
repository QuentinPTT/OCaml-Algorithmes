(* Nombres d'éléments vérifiant une condition *)

let nombre l cond = match l with
	[] -> 0
	|t::q when cond t -> 1+nombre q cond
	|t::q -> nombre q cond;;