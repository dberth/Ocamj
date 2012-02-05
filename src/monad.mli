module type MONAD =
  sig
    type 'a t
    val return: 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
  end

module Monad: functor (M: MONAD) ->
  sig
    type 'a t = 'a M.t

    val return: 'a -> 'a t

    val bind: 'a t -> ('a -> 'b t) -> 'b t

    val seq: 'a t -> 'b t -> 'b t

    val join: 'a t t -> 'a t

    val fmap: ('a -> 'b) -> 'a t -> 'b t

    val liftm: ('a -> 'b) -> 'a t -> 'b t

    val liftm2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    val liftm3: ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

    val mapm: ('a -> 'b) -> 'a t list -> 'b list t

    val sequence: 'a t list -> 'a list t

    val mapm_: ('a -> 'b t) -> 'a list -> unit t

    val sequence_: 'a t list -> unit t

    module Op:
	sig
	  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
	  val (>>): 'a t -> 'b t -> 'b t
	end
  end

module State: functor (S: sig type t end) ->
  sig
    include MONAD

    val get: S.t t
    val set: S.t -> unit t

    val eval: 'a t -> S.t -> 'a
    val run: 'a t -> S.t -> S.t
  end

module StateT: functor (S: sig type t end) -> functor (M: MONAD) ->
  sig
    include MONAD

    val get: S.t t
    val set: S.t -> unit t

    val lift: 'a M.t -> 'a t

    val eval: 'a t -> S.t -> 'a M.t
    val run: 'a t -> S.t -> S.t M.t
  end

module Lwt_state: functor (S: sig type t end) ->
  sig
    include MONAD

    val get: S.t option t
    val set: S.t option -> unit t

    val state: S.t Lwt.key

    val run: 'a t -> S.t option -> 'a Lwt.t
  end
