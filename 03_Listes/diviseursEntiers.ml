(* Liste des diviseurs d'un nombre entier *)

let div n =
	let l=ref [] in
	for k=1 to n do
		if n mod k = 0 then
			l:= k::!l
	done;
	!l;;

div 70;;