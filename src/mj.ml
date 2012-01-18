open Tiles

let hand = {Sets.concealed = [d10; d20; d30; d11; d21; d31; d73; d80; d90; d50; d51; d52; d62; d61]; known = []}

let _ =
   for i = 1 to 10_000 do ignore (ZList.to_list ~limit: 1 (Sets.mahjong hand)) done;
  List.iter
    (fun (sets, _) ->
       print_endline (String.concat "; " (List.map Sets.pp_set sets))
    )
  (ZList.to_list (Sets.mahjong hand));
  List.iter
    (fun (set, _) ->
       print_endline (Sets.pp_set set)
    )
    (ZList.to_list (Sets.chow [d10; d20; d30; d11; d21; d31]))


