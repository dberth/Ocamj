type concealed_tiles =
  | Visible of Tiles.tile list
  | Hidden of int (*number of tiles*)

type player_state =
    {
     wind: Tiles.wind;
     mutable concealed_tiles: concealed_tiles;
     mutable known_sets: Sets.set list;
     mutable discarded_tiles: Tiles.tile list;
   }

type t =
    {
     player_states: player_state array;
     mutable wall_tiles: concealed_tiles;
     mutable current_turn: int;
     mutable current_player: int;
   }

let current_game_state = ref None

let new_player_state wind =
  {
   wind;
   concealed_tiles = Visible [];
   known_sets = [];
   discarded_tiles = [];
 }

let init_wind_of_player = function
  | 0 -> Tiles.East
  | 1 -> Tiles.South
  | 2 -> Tiles.West
  | 3 -> Tiles.North
  | _ -> assert false

type t_ = t
module GSt = Monad.StateT(struct type t = t_ end)(Lwt)
module GStM = Monad.Monad(GSt)
    
let new_game_state () =
  let tiles =
    Random.self_init ();
    let rec aux acc tile =
      if tile < 0 then List.map snd (List.sort compare acc) else
      aux ((Random.float 1., tile) :: acc) (tile - 1)
    in
    aux [] Tiles.max_tile
  in
  {
   player_states = Array.init 4 (fun i -> new_player_state (init_wind_of_player i));
   wall_tiles = Visible tiles;
   current_turn = 0;
 }

let splitn n l =
  let rec aux acc n l =
    if n <= 0 then List.rev acc, l else
    match l with
    | [] -> failwith "splitn"
    | hd :: tl -> aux (hd :: acc) (n - 1) tl
  in
  aux [] n l

let update f =
  let open GStM.Op in
  GSt.get >>= fun state -> f state; GSt.return ()

let update_player_concealed_tiles f i =
  update
    (fun state ->
      match state.player_states.(i).concealed_tiles with
      | Visible concealed_tiles -> state.player_states.(i).concealed_tiles <- Visible (f concealed_tiles)
      | Hidden _ -> assert false
    )

let update_player_known_sets f i =
  update
    (fun state ->
      state.player_states.(i).known_sets <- f (state.player_states.(i).known_sets)
    )

let update_player_discarded_tiles f i =
  update
    (fun state ->
      state.player_states.(i).discarded_tiles <- f(state.player_states.(i).discarded_tiles)
    )

let update_wall_tiles f =
  update
    (fun state ->
      match state.wall_tiles with
      | Visible wall_tiles -> state.wall_tiles <- Visible (f wall_tiles)
      | Hidden _ -> assert false
    )

let get_wall_tiles =
  let open GStM.Op in
  GSt.get >>= fun {wall_tiles; _} ->
    match wall_tiles with
    | Visible x -> GSt.return x
    | Hidden _ -> assert false

let update_current_turn f =
  update (fun state -> state.current_turn <- f state.current_turn)

let update_current_player f =
  update (fun state -W state.current_player <- f state.current_player)

let deal_tiles player nb_tiles =
  let open GStM.Op in
  get_wall_tiles >>= fun wall_tiles ->
    let tiles, rest = splitn nb_tiles wall_tiles in
    update_player_concealed_tiles (fun concealed_tiles -> tiles @ concealed_tiles) player >>
    update_wall_tiles (fun _ -> rest)

let deal =
  let open GStM.Op in
  let rec aux turn player =
    let next_turn, next_player = turn + ((player + 1) / 4), (player + 1) mod 4 in
    if turn = 4 then deal_tiles 0 1 else
    let nb_tiles = if turn = 3 then 1 else 4 in
    deal_tiles player nb_tiles >> aux next_turn next_player 
  in
  aux 0 0

(* let deal_tiles player nb_tiles = *)
(*   match !current_game_state with *)
(*   | None -> assert false *)
(*   | Some state -> *)
(*       match state.wall_tiles with *)
(*       | Visible wall_tiles -> *)
(* 	  let tiles, rest = splitn nb_tiles wall_tiles in *)
(* 	  begin *)
(* 	    match state.player_states.(player).concealed_tiles with *)
(* 	    | Visible concealed_tiles -> *)
(* 		state.player_states.(player).concealed_tiles <- Visible (tiles @ concealed_tiles); *)
(* 		state.wall_tiles <- Visible rest *)
(* 	    | Hidden _ -> assert false *)
(* 	  end *)
(*       | Hidden _ -> assert false *)

(* let deal () = *)
(*   for turn = 0 to 3 do *)
(*     for player = 0 to 3 do *)
(*       let nb_tiles = if turn = 3 then 1 else 4 in *)
(*       deal_tiles player nb_tiles *)
(*     done *)
(*   done; *)
(*   deal_tiles 0 1 *)
      

let hide_tiles = function
  | Visible tiles -> Hidden (List.length tiles)
  | Hidden x -> Hidden x

let hide_player_hand player_state =
  {player_state with
   concealed_tiles = hide_tiles player_state.concealed_tiles
 }

let state_for_player player {player_states; wall_tiles; current_turn; current_player} =
  {
   player_states = Array.init 4 (fun i -> if i = player then player_states.(i) else hide_player_hand player_states.(i));
   wall_tiles = hide_tiles wall_tiles;
   current_turn;
   current_player;
 }
  
module type GAME =
    sig
      val notify_state_to_player: t -> unit Lwt.t
      val player_action_on_turn: player_action_on_turn Lwt.t
    end

module Make(Gi: GAME) =
  struct

    let notify_state_to_players state =
      Lwt.join
	(List.map
	   (fun player ->
	     Gi.notify_state_to_player (state_for_player player state)
	   )
	   [0; 1; 2; 3]
	)

	
    let rec game_loop =
      GSt.get >>= fun state ->
	GSt.lift (notify_state_to_players state) >>
	Gi.player_action_on_turn >>= fun action ->
	  match action with
	  | `Mahjong hand -> handle_mahjong hand
	  | `Kong set -> handle_concealed_kong set
	  | `Discard tile -> handle_discard tile
	  | `Quit -> handle_quit

    and handle_discard tile =
      

    let run () =
      GSt.run (new_game_state ()) >>
      deal >>
      game_loop
  end
