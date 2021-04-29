(* Calcul de Somme *)

(* A - Algorithme récursif classique *)

let rec som n = match n with
		1 -> 1
		|n -> n + som(n-1);;

(* B - Algorithme récursif terminal *)

let som n =
		let rec aux k s = match k with
				1 -> s
				|k -> aux(k-1) (s+k)
		in aux n 1 ;;