open Ctypes

val t : string typ

val is_managed : 'a ptr -> bool
val release : 'a ptr -> unit
