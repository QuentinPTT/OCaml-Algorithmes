(* Recherche d'un élément dans un tableau *)

(* A - Algorithme naif *)

let cherche tab x = 
	let res = ref false in
	let n = Array.length tab in
	for k = 0 to n-1 do
		if tab.(k) = x then
			res:= true
	done;!res;;

(* B - Algorithme astucieux *)

let present tab x =
	let res = ref false in
	let n = Array.length tab in
	let k = ref 0 in
	while not(!res) && !k<n do
		if tab.(!k)=x then
			res:=true;
		incr k;
	done;!res;;

(* C - Algorithme rŽcursif *)

let present2 tab x =
	let n = Array.length tab in
	let rec ch k = match k with
		k when k = n -> false
		|k when tab.(k)=x -> true
		|_ -> ch(k+1)
	in ch 0;;

(* D - Tableau triŽ *)

let cherchetriee tab a =
	let k = ref 0 in
	let n = Array.length tab in
	while !k<=(n-1) && tab.(!k)<a do
		k:=!k+1
	done; !k<n;;
