type 'a _zlist =
  | Cons of ('a * 'a t)
  | Null
and 'a t = 'a _zlist Lazy.t

let empty = lazy Null

let cons x l = lazy (Cons(x, l))

let return x = cons x empty

let msplit zl =
  match Lazy.force zl with
    | Null -> None
    | Cons (hd, tl) -> Some (hd, tl)

let rec concat2 zl1 zl2 =
  match msplit zl1 with
    | None -> zl2
    | Some (hd, tl) -> lazy (Cons(hd, concat2 tl zl2))

let rec interleave2 zl1 zl2 =
  match msplit zl1 with
    | None -> zl2
    | Some (hd, tl) -> lazy (Cons(hd, interleave2 zl2 tl))

let interleave zll =
  List.fold_left
    (fun acc zl ->
       interleave2 zl acc
    )
    empty
    zll

let rec bind_ zl f current_results =
  match current_results with
    | Cons (hd, tl) -> lazy (Cons(hd, bind_ zl f (Lazy.force tl)))
    | Null ->
        match zl with
          | Null -> empty
          | Cons (hd, tl) -> bind_ (Lazy.force tl) f (f hd)
	      
let bind zl f = bind_ (Lazy.force zl) (fun x -> Lazy.force (f x)) Null

let (>>=) = bind

let (>>) zl f = bind zl (fun _ -> f)

let guard test = if test then return () else empty

let of_list l =
  List.fold_left
    (fun acc x -> bind (return x) (fun x -> lazy (Cons(x, acc))))
    empty
    l
    
let to_list ?(limit = -1) zl =
  let rec aux acc zl cpt =
    if cpt = 0 then acc else
      match zl with
	| Null -> acc
	| Cons (hd, tl) -> (aux (hd :: acc) (Lazy.force tl) (cpt - 1))
  in
    List.rev (aux [] (Lazy.force zl) limit)

let rec choose prec acc l =
    match l with
      | [] -> acc
      | hd :: tl ->
	  let acc =
            cons (hd, List.rev_append prec tl) acc
	  in
	    choose (hd :: prec) acc tl
	      
let choose x = choose [] empty x

let rec hd zl =
  match Lazy.force zl with
    | Null -> empty
    | Cons (x, _) -> return x
