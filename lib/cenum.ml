module type T = sig
  type t [@@deriving enumerate, compare, sexp]
end

type 'a t = {
  total : 'a C.typ;
  partial : 'a option C.typ;
}

let define ?(first=0) (type t) (module E: T with type t = t) name =
  assert (first >= 0);
  let enum = Array.of_list E.all in
  Array.sort enum ~cmp:E.compare;
  let read i = Array.get enum (i - first) in
  let write t =
    match
      Array.binary_search ~compare:E.compare enum `First_equal_to t
    with None -> assert false
       | Some x -> x + first in
  let read_opt i = if i = -1  then None else Some (read i) in
  let write_opt = function
    | None -> -1
    | Some x -> write x in
  let pref = String.uppercase ("bap_" ^ name) in
  let view read write =
    C.view ~format_typ:(fun k ppf -> fprintf ppf "enum %s_tag%t" name k)
      ~read ~write C.int in
  let cases = List.mapi E.all ~f:(fun i x ->
      let name = String.uppercase (Sexp.to_string (E.sexp_of_t x)) in
      pref ^ "_" ^ name, Int64.of_int (first + i)) in
  let cases = (pref ^ "_INVALID", Int64.of_int (~-1)) :: cases in
  let t = view read write in
  let partial = view read_opt write_opt in
  Internal.enum cases t;
  Internal.typedef t (name ^ "_tag");
  {total = t; partial}

let total t = t.total
let partial t = t.partial
