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
   }

val new_game_state: unit -> t

val state_for_player: int -> t
