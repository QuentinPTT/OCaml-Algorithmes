(* Calcul de la somme des n premiers entiers *)

let somme n = 
		let s = ref 0 in
		for k = 1 to n do
				s:= !s + k
		done;
		!s;;