type style = [`snake_case | `CamelCase | `SNAKE_CASE]

type t

type cident = private string

val create : ?prefix:string -> ?suffix:string -> ?style:style -> string -> t
val compare : t -> t -> int
val name : t -> cident
val prefix : t -> string option
val suffix : t -> string option
val append : t -> string -> t
