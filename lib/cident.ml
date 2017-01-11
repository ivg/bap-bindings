open Core_kernel.Std

type style = [`snake_case | `CamelCase | `SNAKE_CASE]

type cident = string

type t = {
  repr : string;
  orig : string list;
  pref : string option;
  suff : string option;
  case : style;
}


let cliterals = [
  Char.is_alphanum;
  fun c -> c = '_';
]

let is_cliteral c = List.exists cliterals ~f:(fun f -> f c)

let encode = String.concat_map ~f:(fun c ->
    if is_cliteral c then String.of_char c
    else match c with
      | '-' -> "_"
      | c -> sprintf "__%02x" (Char.to_int c))


let join ~sep ~style pref suff idents =
  let opt = Option.value_map ~default:[] ~f:(fun x ->
      [style (encode x)]) in
  String.concat ~sep @@ List.concat [
    opt pref;
    List.map idents ~f:(fun ident -> style (encode ident));
    opt suff
  ]


let make = function
  | `snake_case -> join ~sep:"_" ~style:String.lowercase
  | `CamelCase ->  join ~sep:"" ~style:String.capitalize
  | `SNAKE_CASE -> join ~sep:"_" ~style:String.uppercase

let create ?prefix ?suffix ?(style=`snake_case) orig =
  {
    repr = make style prefix suffix [orig];
    orig = [orig]; suff=suffix; pref=prefix;
    case = style;
  }

let name t = t.repr
let prefix t = t.pref
let suffix t = t.suff
let compare x y = String.compare x.repr y.repr

let append x name = {
  x with
  repr = make x.case x.pref x.suff (x.orig @ [name]);
  orig = x.orig @ [name];
}
