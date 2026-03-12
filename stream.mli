type 'a t = Cons of 'a * 'a t lazy_t | Empty

val first : default:'a -> 'a t -> 'a
val to_stream : 'a list -> 'a t
val singleton : 'a -> 'a t
val count : 'a t -> int
val drop_while : ('a -> bool) -> 'a t -> 'a t
val take_while : ('a -> bool) -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit
val filter : ('a -> bool) -> 'a t -> 'a t
val cat : 'a t -> 'a t -> 'a t
val flatmap : ('a -> 'b t) -> 'a t -> 'b t
