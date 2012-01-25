type serie =
  | Dot
  | Bam
  | Char

type color =
  | White
  | Green
  | Red

type wind =
  | East
  | South
  | West
  | North

type tile_descr =
  | Serie of serie * int
  | Dragon of color
  | Wind of wind

type basic_tile_descr = tile_descr * int

type tile = int

val tile_of_descr: ?instance: int -> tile_descr -> tile

val descr_of_tile: tile -> basic_tile_descr

val pp_tile: ?show_instance: bool -> tile -> string

val d10: tile
val d11: tile
val d12: tile
val d13: tile
val d20: tile
val d21: tile
val d22: tile
val d23: tile
val d30: tile
val d31: tile
val d32: tile
val d33: tile
val d40: tile
val d41: tile
val d42: tile
val d43: tile
val d50: tile
val d51: tile
val d52: tile
val d53: tile
val d60: tile
val d61: tile
val d62: tile
val d63: tile
val d70: tile
val d71: tile
val d72: tile
val d73: tile
val d80: tile
val d81: tile
val d82: tile
val d83: tile
val d90: tile
val d91: tile
val d92: tile
val d93: tile
val b10: tile
val b11: tile
val b12: tile
val b13: tile
val b20: tile
val b21: tile
val b22: tile
val b23: tile
val b30: tile
val b31: tile
val b32: tile
val b33: tile
val b40: tile
val b41: tile
val b42: tile
val b43: tile
val b50: tile
val b51: tile
val b52: tile
val b53: tile
val b60: tile
val b61: tile
val b62: tile
val b63: tile
val b70: tile
val b71: tile
val b72: tile
val b73: tile
val b80: tile
val b81: tile
val b82: tile
val b83: tile
val b90: tile
val b91: tile
val b92: tile
val b93: tile
