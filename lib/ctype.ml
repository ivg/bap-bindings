type ('a,'b) t = {
  t : 'a C.typ;
  o : 'b;
  id : int;
  base : string;
  prefix : string;
  suffix : string;
}

type style = [`CamelCase | `snake_case ]

let create
    ?(prefix="")
    ?(suffix="")
    ?(style=`snake_case) name = ()


type 'a nullable = ('a, 'a option) Ctype.t
type 'a total = ('a, unit) Ctype.t
