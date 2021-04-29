(* Nombre parfait *)

let rec somme l = match l with
	[] -> 0
	|t::q -> t + somme q;;
	
let div n =
	let l=ref [] in
	for k=1 to n do
		if n mod k = 0 then
			l:= k::!l
	done;
	!l;;

let parfait n = somme (div n) = 2*n;;

parfait 8;;