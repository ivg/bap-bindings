module type T = sig
  type t [@@deriving enumerate, compare, sexp]
end
type 'a t
val define : ?first:int -> (module T with type t = 'a) -> string -> 'a t
val total : 'a t -> 'a C.typ
val partial : 'a t -> 'a option C.typ
