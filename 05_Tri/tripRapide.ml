(* Tri Rapide *)

(* A - Version Tableau *)

let echange t i j=let a=t.(i) in 
   t.(i)<-t.(j); t.(j)<-a;;

let partition t a b=
   let p=t.(a) in let c=ref a and f=ref b in
       while !c< !f do match t.(!c+1)<p with
           true -> echange t !c (!c+1);
                   incr c
          |_ -> echange t (!c+1) !f;
                 decr f
        done;
			!c;;

let triRap t=let n=Array.length t in 
    let rec triRec a b= if a<b then
        let c=partition t a b in
           triRec a (c-1);
           triRec (c+1) b
   in triRec 0 (n-1);
   t;;

(* B - Version Liste *)

let rec sep l p = match l with
	|[] -> [],[]
	|[a] when a<p -> [a],[]
	|[a] -> [],[a]
	|t::q when t<p -> t::(fst(sep q p)),snd(sep q p)
	|t::q -> fst(sep q p),t::(snd(sep q p));;

let rec triRap l = match l with
	[] -> []
	|t::q -> let l1,l2 = sep q t in
			(triRap l1)@(t::(triRap l2));;