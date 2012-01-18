type concealed_tiles =
  | Visible of Tile.tile list
  | Hidden of int (*number of tiles*)

type player_state =
    {
     wind: Tile.wind;
     mutable concealed_tiles;
     known_sets: Sets.set list;
     discarded_tiles: Tile.tile list;
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
   concealed_tiles = [];
   known_sets = Visible [];
   discarded_tiles = [];
 }

let init_wind_of_player = function
  | 0 -> East
  | 1 -> South
  | 2 -> West
  | 3 -> Noth
  | _ -> assert false
    
let new_game () =
  let tiles =
    Random.self_init ();
    let rec aux acc tile =
      if tile < 0 then List.map snd (List.sort compare acc) else
      aux ((Random.float 1., tile) :: acc) (tile - 1)
    in
    aux Tiles.max_tile
  in
  current_game :=
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
  match !current_state with
  | None -> assert false
  | Some state ->
      let tiles, rest = splitn nb_tiles state.wall_tiles in
      state.player_states.(player).concealed_tiles <- tiles :: state.player_states.(player).concealed_tiles;
      state.wall_tiles <- rest

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

let state_for_player player {player_states; wall_tiles; current_turn} =
  {
   player_states = Array.init 4 (fun i -> if i = player then player_states.(i) else hide_player_hand player_state.(i));
   wall_tiles = hide_tiles wall_tiles;
   current_turn;
 }
  
