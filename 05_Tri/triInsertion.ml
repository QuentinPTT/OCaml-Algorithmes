(* Tri par insertion *)

let tableau = [|10;3;6;2;4;1|];;
let liste = [10;3;6;2;4;1];;

(* A - Version Tableau *)

let triInsertionTab tab =
		let n = Array.length(tab) in
		for k = 1 to n-1 do
				let x = tab.(k) in
				let j = ref(k-1) in
				while !j>=0 && tab.(!j) > x do
						tab.(!j+1)<-tab.(!j);
						decr j;
				done;
				tab.(!j+1)<-x;
		done;
		tab;;
		
triInsertionTab tableau;;

(* B - Version Liste *)

let rec insertion x l = match l with
		|[] -> [x]
		|t::q when x<t -> x::l
	  |t::q -> t::(insertion x q);;
	 
let rec triListe l = match l with
		|[] -> []
		|t::q -> insertion t (triListe q);;
		
triListe liste;;
