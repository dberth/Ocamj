open Tiles

type set_kind =
  | Pair
  | Chow
  | Pung
  | Kong
  | Other

type set =
    {
      tiles: tile list;
      kind: set_kind;
      concealed: bool;
    }

let build_set ?(concealed = true) tiles kind =
  {tiles; kind; concealed}

let pp_set_kind = function
  | Pair -> "P2"
  | Chow -> "C"
  | Pung -> "P3"
  | Kong -> "K"
  | Other -> "?"

let pp_set {tiles; kind; concealed} =
  let lpar, rpar =
    if concealed then "(",")" else "{", "}"
  in
    Printf.sprintf "%s%s%s%s" (pp_set_kind kind) lpar (String.concat ", " (List.map (pp_tile ~show_instance: true) tiles)) rpar  

let is_same t1 t2 = if t1 < t2 && t1 / 4 = t2 / 4 then ZList.return () else ZList.empty

let can_be_chow  t1 t2 =
  if t1 < 108 && t2 < 108 then
    let delta = t2 / 4 - t1 / 4 in 
    if delta = 1 || delta = 2 then ZList.return () else ZList.empty
  else
    ZList.empty
 
let is_chow t1 t2 t3 =
  if t1 < 108 && t2 < 108 && t3 < 108 then
    if (t2 / 4 - t1 / 4 = 1) && (t3 / 4 - t2 / 4 = 1) then ZList.return () else ZList.empty 
  else
    ZList.empty

(*better if tile sorted in decreasing order*)

let pair hand =
  let open ZList in
    choose hand >>= fun (t1, rest) -> choose rest >>= fun (t2, rest) ->
      is_same t1 t2 >> return (build_set [t1; t2] Pair, rest)

let pung hand =
  let open ZList in
    choose hand >>= fun (t1, rest) -> choose rest >>= fun (t2, rest) ->
      is_same t1 t2 >> choose rest >>= fun (t3, rest) ->
	is_same t2 t3 >> return (build_set [t1; t2; t3] Pung, rest)

let kong hand =
  let open ZList in
    choose hand >>= fun (t1, rest) -> choose rest >>= fun (t2, rest) ->
      is_same t1 t2 >> choose rest >>= fun (t3, rest) ->
	is_same t2 t3 >> choose rest >>= fun (t4, rest) ->
          is_same t3 t4 >> return (build_set [t1; t2; t3; t4] Kong, rest)

let chow hand =
  let open ZList in
    choose hand >>= fun (t1, rest) -> choose rest >>= fun (t2, rest) ->
      can_be_chow t1 t2 >> choose rest >>= fun (t3, rest) ->
	is_chow t1 t2 t3 >> return (build_set [t1; t2; t3] Chow, rest)

let rec calculate_set nb kind acc prec prev = function
  | [] -> acc
  | hd :: tl ->
      let acc, prec, prev =
	match prev with
	  | [] -> acc, prec, [hd]
	  | hd_prev :: _ ->
	      if hd / 4 = hd_prev / 4 then
		let prev = hd :: prev in
		  if List.length prev = nb then
		      ZList.cons (build_set (List.rev prev) kind, List.rev_append prec tl) acc, List.rev_append prev prec, []
		  else
		    acc, prec, prev
	      else
		acc, List.rev_append prev prec, [hd]
      in
      calculate_set nb kind acc prec prev tl

let set_same nb kind hand = (*print_endline "Same";*) calculate_set nb kind ZList.empty [] [] hand

let pair hand = set_same 2 Pair hand

let pung hand = set_same 3 Pung hand

let kong hand = set_same 4 Kong hand

let rec chow acc prev hand l =
  (*print_endline (String.concat ", " (List.map string_of_int prev));*)
  match l with
  | [] -> acc
  | hd :: tl ->
      match prev with
	| [] -> chow acc [hd] hand tl
	| hd_prev :: _ ->
	    begin
	      if hd / 4 = hd_prev / 4 then
		chow acc prev hand tl
	      else if hd / 4 - hd_prev / 4 = 1 then
		let acc =
		  match prev with
		    | [_] -> acc
		    | prev1 :: prev2 :: _ ->
			ZList.cons (build_set [prev2; prev1; hd] Chow, List.filter (fun x -> x <> prev1 && x <> prev2 && x <> hd) hand) acc
 		    | _ -> assert false
 		in
		  chow acc (hd :: prev) hand tl
	      else
		chow acc [hd] hand tl
	    end

	      

let chow hand = (*print_endline "Chow";*) let hand = List.sort compare hand in chow ZList.empty [] hand hand

let set hand =
  let open ZList in
    interleave [kong hand; pung hand; chow hand]
      
let mahjong hand =
  let hand = List.sort compare hand in
  let open ZList in
    set hand >>= fun (set1, rest) -> (*print_endline (Printf.sprintf "==== set 1: %s" (pp_set set1));*) set rest >>= fun (set2, rest) ->
      ((*print_endline (Printf.sprintf "==== set 2: %s %s" (pp_set set1) (pp_set set2));*) set rest ) >>= fun (set3, rest) ->
      ((*print_endline (Printf.sprintf "==== set 3: %s %s %s %s" (pp_set set1)  (pp_set set2) (pp_set set3) (String.concat "; " (List.map string_of_int rest)));*) set rest ) >>= fun (set4, rest) ->
      ((*print_endline (Printf.sprintf "==== set 4: %s %s %s %s" (pp_set set1) (pp_set set2) (pp_set set3) (pp_set set4));*) pair rest ) >>= fun (pair, rest) -> (*print_endline "OK";*) return ([set1; set2; set3; set4; pair], rest)
	    
