module C = Ctypes

type 'a t

val newtype : ?prefix:string -> ?suffix:string -> string -> 'a t
val view : 'a t -> read:('a -> 'b) -> write:('b -> 'a) -> 'b t
val total : 'a t -> 'a C.typ
val nullable : 'a t -> 'a option C.typ
val instanceof : base:'a t -> 'b t -> unit
val typename : 'a t -> string
val name : 'a t -> string
