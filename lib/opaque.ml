open Core_kernel.Std

open C

module C = Ctypes

type 'a t = {
  total  : 'a C.typ;
  nullable  : 'a option C.typ;
  instances : Int.Hash_set.t;
  id : int;
  base : string;
  prefix : string;
  suffix : string;
}

type 'a fat = {
  typeid : int;
  ovalue : 'a;
}

type typeinfo = {
  name : string;
}

type opaque_t = Opaque

let registered = ref 0
let typeinfo = Int.Table.create ~size:1024 ()

let type_error name id =
  let got = match Hashtbl.find typeinfo id with
    | None -> "<unknown>"
    | Some {name} -> name in
  invalid_argf
    "Type error: expected a value of type %s, but got %s"
    name got ()

let nullpointer name =
  invalid_argf
    "Fatal error: an attempt to dereference a \
     null pointer of type %s" name ()

let addr ptr = raw_address_of_ptr (to_voidp ptr)

type cstruct = opaque_t structure

let nullable t = t.nullable
let total t = t.total
let typename t = t.prefix ^ t.base ^ t.suffix
let name t = t.base

let newtype (type t) ?(prefix="bap_") ?(suffix="_t") base =
  incr registered;
  let name = prefix ^ base ^ suffix in
  let t : cstruct typ = structure name in
  let typeid = !registered in
  let null = from_voidp t null in
  Internal.typedef t name;
  Hashtbl.set typeinfo ~key:typeid ~data:{name};
  let instances = Int.Hash_set.create () in
  let is_instance id =
    id = typeid || Hash_set.mem instances id in
  let read (opaque : cstruct ptr) : t =
    if is_null opaque then nullpointer name;
    let {typeid=id; ovalue} = Root.get (to_voidp opaque) in
    if is_instance id then ovalue
    else type_error name id in
  let write (ovalue : t) : cstruct ptr =
    from_voidp t (Root.create {typeid; ovalue}) in
  let read_opt ptr = if is_null ptr then None else Some (read ptr) in
  let write_opt = Option.value_map ~f:write ~default:null in
  let view read write = view ~read ~write (ptr t) in
  {
    total = view read write;
    nullable = view read_opt write_opt;
    instances; id=typeid; base;
    prefix; suffix;
  }

let instanceof ~base t =
  Hash_set.add base.instances t.id

let view t ~read ~write = {
  t with
  total = C.view t.total ~read ~write;
  nullable = C.view t.nullable
      ~read:(Option.map ~f:read)
      ~write:(Option.map ~f:write);
}
