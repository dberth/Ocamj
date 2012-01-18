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
val c10: tile
val c11: tile
val c12: tile
val c13: tile
val c20: tile
val c21: tile
val c22: tile
val c23: tile
val c30: tile
val c31: tile
val c32: tile
val c33: tile
val c40: tile
val c41: tile
val c42: tile
val c43: tile
val c50: tile
val c51: tile
val c52: tile
val c53: tile
val c60: tile
val c61: tile
val c62: tile
val c63: tile
val c70: tile
val c71: tile
val c72: tile
val c73: tile
val c80 : tile
val c81 : tile
val c82 : tile
val c83 : tile
val c90 : tile
val c91 : tile
val c92 : tile
val c93 : tile
val wd0 : tile
val wd1 : tile
val wd2 : tile
val wd3 : tile
val gd0 : tile
val gd1 : tile
val gd2 : tile
val gd3 : tile
val rd0 : tile
val rd1 : tile
val rd2 : tile
val rd3 : tile
val ew0 : tile
val ew1 : tile
val ew2 : tile
val ew3 : tile
val sw0 : tile
val sw1 : tile
val sw2 : tile
val sw3 : tile
val ww0 : tile
val ww1 : tile
val ww2 : tile
val ww3 : tile
val nw0 : tile
val nw1 : tile
val nw2 : tile
val nw3 : tile
