open Tiles

(* let hand = {Sets.concealed = [d10; d20; d30; d11; d21; d31; d73; d80; d90; d50; d51; d52; d62; d61]; known = []} *)

(* let _ = *)
(*    for i = 1 to 10_000 do ignore (ZList.to_list ~limit: 1 (Sets.mahjong hand)) done; *)
(*   List.iter *)
(*     (fun (sets, _) -> *)
(*        print_endline (String.concat "; " (List.map Sets.pp_set sets)) *)
(*     ) *)
(*   (ZList.to_list (Sets.mahjong hand)); *)
(*   List.iter *)
(*     (fun (set, _) -> *)
(*        print_endline (Sets.pp_set set) *)
(*     ) *)
(*     (ZList.to_list (Sets.chow [d10; d20; d30; d11; d21; d31])) *)

module St = Monad.StateT(struct type t = int end)(Lwt)

let incr = St.bind St.get (fun s -> St.set (succ s))

let ( +! ) mx my =
  St.bind mx   (fun x ->
  St.bind my   (fun y ->
  St.bind incr (fun _ -> St.return (x + y))))

let test () =
  let msum l = List.fold_right (fun x m -> St.return x +! m) l (St.return 0)
  in
  let module M = Monad.Monad(St) in
  let open M.Op in
    St.eval (msum [1;2;3;4] >>= fun x -> print_int x; St.get >>= (fun x -> print_int x; St.return ())) 0

let _ = test ()


