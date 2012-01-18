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
      is_concealed: bool;
    }

let build_set ?(concealed = true) tiles kind =
  {tiles; kind; is_concealed = concealed}

type hand =
    {
     concealed: tile list;
     known: set list;
    }

let pp_set_kind = function
  | Pair -> "P2"
  | Chow -> "C"
  | Pung -> "P3"
  | Kong -> "K"
  | Other -> "?"

let pp_set {tiles; kind; is_concealed} =
  let lpar, rpar =
    if is_concealed then "(",")" else "{", "}"
  in
    Printf.sprintf "%s%s%s%s" (pp_set_kind kind) lpar (String.concat ", " (List.map (pp_tile ~show_instance: true) tiles)) rpar

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

let set_same nb kind hand = calculate_set nb kind ZList.empty [] [] hand

let pair hand = set_same 2 Pair hand

let pung hand = set_same 3 Pung hand

let kong hand = set_same 4 Kong hand

let rec chow acc prev hand l =
  (*print_endline (String.concat ", " (List.map string_of_int prev));*)
  match l with
  | [] -> acc
  | hd :: tl ->
      if hd < 108 then
	match prev with
	  | [] -> chow acc [hd] hand tl
	  | hd_prev :: _ ->
	      begin
		if hd / 4 = hd_prev / 4 then
		  chow acc prev hand tl
		else if hd / 36 = hd_prev / 36 && hd / 4 - hd_prev / 4 = 1 then
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
      else
	acc

let chow hand = chow ZList.empty [] hand hand

let set {concealed; known} =
  let open ZList in
  match known with
  | [] ->
      interleave [kong concealed; pung concealed; chow concealed] >>= fun (set, concealed) -> return (set, {concealed; known})
  | _ ->
    choose known >>= fun (set, known) -> return (set, {concealed; known})

let pair {concealed; known} =
  let open ZList in
  pair concealed >>= fun (pair, concealed) -> return (pair, {concealed; known})

let mahjong hand =
  let hand =
    {hand with concealed = List.sort compare hand.concealed}
  in
  let open ZList in
    set hand >>= fun (set1, rest) ->
      set rest >>= fun (set2, rest) ->
        set rest >>= fun (set3, rest) ->
          set rest >>= fun (set4, rest) ->
            pair rest >>= fun (pair, rest) ->
              return ([set1; set2; set3; set4; pair], rest)

