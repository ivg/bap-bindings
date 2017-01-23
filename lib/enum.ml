open Core_kernel.Std

module type T = sig
  type t [@@deriving enumerate, compare, sexp]
end

type 'a t = {
  total : 'a C.typ;
  partial : 'a option C.typ;
}


let define ?(invalid="invalid")
    ?(first=0) (type t) (module E: T with type t = t) name =
  assert (first >= 0);
  let compare x y = E.compare y x in
  let enum = Array.of_list E.all in
  Array.sort enum ~cmp:compare;
  let read i =
    Array.get enum (i - first) in
  let write t =
    match
      Array.binary_search ~compare enum `First_equal_to t
    with None -> assert false
       | Some x -> x + first in
  let read_opt i = if i = -1  then None else Some (read i) in
  let write_opt = function
    | None -> -1
    | Some x -> write x in
  let pref = String.uppercase ("bap_" ^ name) in
  let view read write =
    C.view ~format_typ:(fun k ppf -> fprintf ppf "enum bap_%s_t%t" name k)
      ~read ~write C.int in
  let cases = Array.to_list enum |> List.mapi  ~f:(fun i x ->
      let name = String.uppercase (Sexp.to_string (E.sexp_of_t x)) in
      pref ^ "_" ^ name, Int64.of_int (first + i)) in
  let invalid = String.uppercase invalid in
  let cases = (pref ^ "_" ^ invalid, Int64.of_int (~-1)) :: cases in
  let t = view read write in
  let partial = view read_opt write_opt in
  Internal.enum cases t;
  Internal.typedef t ("bap_" ^ name ^ "_t");
  {total = t; partial}

let total t = t.total
let partial t = t.partial
