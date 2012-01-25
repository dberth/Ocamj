type 'a t

val empty: 'a t

val cons: 'a -> 'a t -> 'a t

val return: 'a -> 'a t

val bind: 'a t -> ('a -> 'b t) -> 'b t

val (>>=): 'a t -> ('a -> 'b t) -> 'b t

val (>>): 'a t -> 'b t -> 'b t

val interleave: 'a t list -> 'a t

val guard: bool -> unit t

val choose: 'a list -> ('a * 'a list) t

val hd: 'a t -> 'a t

val of_list: 'a list -> 'a t

val to_list: ?limit: int -> 'a t -> 'a list
