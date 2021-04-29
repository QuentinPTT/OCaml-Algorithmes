(* Tri fusion *)

(* A - Version Tableau *)

let fusion t aux a b c=
	let g=ref a and d=ref (c+1) in
   	let k=ref 0 in
   	while !g<=c && !d<=b do match t.(!g)<t.(!d) with
       		true -> aux.(!k)<-t.(!g);
              		incr g;
              		incr k
      		| _ -> aux.(!k)<-t.(!d);
          	       incr d;
              	       incr k
    	done;
    	while !g<=c do  aux.(!k)<-t.(!g);
        	incr g;
        	incr k done;
   	while !d<=b do aux.(!k)<-t.(!d);
        	incr d;
		incr k done;
    	for i=0 to b-a do
      		t.(a+i)<-aux.(i) done;;
		
let aux=Array.make 10 0;; 
fusion ex aux 1 7 3;;
ex;;

let triFus t=let n=Array.length t in
let aux=Array.make n t.(0) in
	let rec triR d f= match d<f with
    		false -> ()
    		|_ -> let m=(d+f)/2 in
       			triR d m;
       			triR (m+1) f;
       			fusion t aux d f m
	in triR 0 (n-1);
  	t;;
  
let exple=[|4;2;6;0;1;6;10;13|];;
triFus exple;;

(* B - Version Liste *)

let rec separe l = match l with
	[] -> [], []
	|[a] -> [a],[]
	|a::b::q -> let l1,l2 = separe q in a::l1,b::l2;;

let rec fusion l1 l2 = match l1,l2 with
	[],_ -> l2
	|_,[] -> l1
	|t1::q1,t2::q2 when t1<t2 -> t1::(fusion q1 l2)
	|t1::q1,t2::q2 -> t2::(fusion l1 q2);;

let rec triFusion l = match l with
	[] -> []
	|[a] -> [a]
	|_ -> let l1,l2 = separe l in fusion (triFusion l1) (triFusion l2);;
