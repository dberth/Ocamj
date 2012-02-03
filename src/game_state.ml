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
     mutable current_discard: Tiles.tile option;
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
   current_discard = None;
   current_player = 0;
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
  update (fun state -> state.current_player <- f state.current_player)

let update_current_discard f =
  update (fun state -> state.current_discard)

let deal_tiles player nb_tiles =
  let open GStM.Op in
  get_wall_tiles >>= fun wall_tiles ->
    try
      let tiles, rest = splitn nb_tiles wall_tiles in
      update_player_concealed_tiles (fun concealed_tiles -> tiles @ concealed_tiles) player >>
      update_wall_tiles (fun _ -> rest) >> GSt.return true
    with
    | Failure _ -> GSt.return false

let deal =
  let open GStM.Op in
  let rec aux turn player =
    let next_turn, next_player = turn + ((player + 1) / 4), (player + 1) mod 4 in
    if turn = 4 then GSt.return () else
    let nb_tiles = if turn = 3 then 1 else 4 in
    deal_tiles player nb_tiles >> aux next_turn next_player 
  in
  aux 0 0

let hide_tiles = function
  | Visible tiles -> Hidden (List.length tiles)
  | Hidden x -> Hidden x

let hide_player_hand player_state =
  {player_state with
   concealed_tiles = hide_tiles player_state.concealed_tiles
 }

let state_for_player player {player_states; wall_tiles; current_turn; current_player; current_discard} =
  {
   player_states = Array.init 4 (fun i -> if i = player then player_states.(i) else hide_player_hand player_states.(i));
   wall_tiles = hide_tiles wall_tiles;
   current_turn;
   current_player;
   current_discard;
 }

type player_action_on_turn =
  | T_mahjong
  | T_kong of Sets.set
  | T_discard of Tiles.tile
  | T_quit

type players_action_on_discard =
  | D_mahjong of Sets.hand
  | D_kong of Sets.set
  | D_pung of Sets.set
  | D_show of Sets.set
  | D_quit
    
module type GAME =
    sig
      val notify_state_to_player: int -> t -> unit Lwt.t
      val player_action_on_turn: player_action_on_turn Lwt.t
      val players_action_on_discard: (int * players_action_on_discard) option Lwt.t
      val end_of_game_no_more_tiles: unit Lwt.t
      val notify_winner: int -> unit Lwt.t
      val notify_player_retire: int -> unit Lwt.t
    end

module Make(Gi: GAME) =
  struct

    let notify_state_to_players state =
      Lwt.join
	(List.map
	   (fun player ->
	     Gi.notify_state_to_player player (state_for_player player state)
	   )
	   [0; 1; 2; 3]
	)

    let get_and_notify_state =
      let open GStM.Op in
      GSt.get >>= fun state -> GSt.lift (notify_state_to_players state) >> GSt.return state
	
    let rec game_loop () =
      let open GStM.Op in
      GSt.get >>= fun state ->
	deal_tiles (state.current_player) 1 >>= fun continue ->
	  if continue then
	    GSt.lift (notify_state_to_players state) >>
	    GSt.lift Gi.player_action_on_turn >>= fun action ->
	      match action with
	      | T_discard tile -> handle_discard tile
	      | T_kong set -> handle_concealed_kong_declaration set
	      | T_mahjong -> handle_mahjong_on_turn
	      | T_quit -> handle_quit
	  else
	    GSt.lift Gi.end_of_game_no_more_tiles

    and handle_discard tile =
      let open GStM.Op in
      GSt.get >>= fun state ->
	update_current_discard (fun _ -> Some tile) >>
	update_player_concealed_tiles (fun tiles -> List.filter (fun x -> x <> tile) tiles) state.current_player >>
	GSt.lift (notify_state_to_players state) >>
	GSt.lift Gi.players_action_on_discard >>= fun action ->
	  match action with
	  | None ->
	      update_player_discarded_tiles (fun tiles -> tile :: tiles) state.current_player >>
	      update_current_discard (fun _ -> None) >>
	      update_current_player (fun player -> (succ player) mod 4) >>
	      GSt.lift (notify_state_to_players state) >> game_loop ()
	  | Some (player, action) -> assert false

    and handle_concealed_kong_declaration set =
      let open GStM.Op in
      GSt.get >>= fun state ->
	update_player_known_sets (fun known_sets -> set :: known_sets) state.current_player >>
	update_player_concealed_tiles (fun tiles -> List.filter (fun tile -> not (List.mem tile set.Sets.tiles)) tiles) state.current_player >>
	GSt.lift (notify_state_to_players state) >> game_loop ()

    and handle_mahjong_on_turn =
      let open GStM.Op in
      GSt.get >>= fun state ->
	GSt.lift (Gi.notify_winner state.current_player)

    and handle_quit =
      let open GStM.Op in
      GSt.get >>= fun state ->
	GSt.lift (Gi.notify_player_retire state.current_player)
      

    let run () =
      let open GStM.Op in
      GSt.run (deal >> game_loop ()) (new_game_state ())
  end
