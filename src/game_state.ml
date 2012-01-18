type concealed_tiles =
  | Visible of Tiles.tile list
  | Hidden of int (*number of tiles*)

type player_state =
    {
     wind: Tiles.wind;
     mutable concealed_tiles: concealed_tiles;
     known_sets: Sets.set list;
     discarded_tiles: Tiles.tile list;
   }

type t =
    {
     player_states: player_state array;
     mutable wall_tiles: concealed_tiles;
     mutable current_turn: int;
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
    
let new_game () =
  let tiles =
    Random.self_init ();
    let rec aux acc tile =
      if tile < 0 then List.map snd (List.sort compare acc) else
      aux ((Random.float 1., tile) :: acc) (tile - 1)
    in
    aux [] Tiles.max_tile
  in
  current_game_state :=
    Some
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
      

let deal_tiles player nb_tiles =
  match !current_game_state with
  | None -> assert false
  | Some state ->
      match state.wall_tiles with
      | Visible wall_tiles ->
	  let tiles, rest = splitn nb_tiles wall_tiles in
	  begin
	    match state.player_states.(player).concealed_tiles with
	    | Visible concealed_tiles ->
		state.player_states.(player).concealed_tiles <- Visible (tiles @ concealed_tiles);
		state.wall_tiles <- Visible rest
	    | Hidden _ -> assert false
	  end
      | Hidden _ -> assert false

let deal () =
  for turn = 0 to 3 do
    for player = 0 to 3 do
      let nb_tiles = if turn = 3 then 1 else 4 in
      deal_tiles player nb_tiles
    done
  done;
  deal_tiles 0 1
      

let hide_tiles = function
  | Visible tiles -> Hidden (List.length tiles)
  | Hidden x -> Hidden x

let hide_player_hand player_state =
  {player_state with
   concealed_tiles = hide_tiles player_state.concealed_tiles
 }

let state_for_player_ player {player_states; wall_tiles; current_turn} =
  {
   player_states = Array.init 4 (fun i -> if i = player then player_states.(i) else hide_player_hand player_states.(i));
   wall_tiles = hide_tiles wall_tiles;
   current_turn;
 }
  
let state_for_player player =
  match !current_game_state with
  | None -> assert false
  | Some state -> state_for_player_ player state
