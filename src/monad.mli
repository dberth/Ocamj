module type MONAD =
  sig
    type 'a m
    val return: 'a -> 'a m
    val bind: 'a m -> ('a -> 'b m) -> 'b m
  end

module Monad: functor (M: MONAD) ->
  sig
    type 'a m = 'a M.m

    val return: 'a -> 'a m

    val bind: 'a m -> ('a -> 'b m) -> 'b m

    val seq: 'a m -> 'b m -> 'b m

    val join: 'a m m -> 'a m

    val fmap: ('a -> 'b) -> 'a m -> 'b m

    val liftm: ('a -> 'b) -> 'a m -> 'b m

    val liftm2: ('a -> 'b -> 'c) -> 'a m -> 'b m -> 'c m

    val liftm3: ('a -> 'b -> 'c -> 'd) -> 'a m -> 'b m -> 'c m -> 'd m

    val mapm: ('a -> 'b) -> 'a m list -> 'b list m

    val sequence: 'a m list -> 'a list m

    val mapm_: ('a -> 'b m) -> 'a list -> unit m

    val sequence_: 'a m list -> unit m

    module Op:
	sig
	  val (>>=): 'a m -> ('a -> 'b m) -> 'b m
	  val (>>): 'a m -> 'b m -> 'b m
	end
  end

module State: functor (S: sig type t end) ->
  sig
    include MONAD

    val get: S.t m
    val set: S.t -> unit m

    val eval: 'a m -> S.t -> 'a
    val run: 'a m -> S.t -> S.t
  end
