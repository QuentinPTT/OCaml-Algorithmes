(* Recherche dichotomique *)

let dicho tab a=
	let rec dic tab d f =
		if d>f then false
		else if d=f then tab.(d) = a
			else let m =(d+f)/2 in
				if tab.(m)=a then true
					else if tab.(m)>a then dic tab d m
						else dic tab (m+1) f
	in dic tab 0 ((Array.length tab)-1);;