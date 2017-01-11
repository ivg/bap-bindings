module Array = C.CArray
module Pool = Nativeint.Table

let managed = Pool.create ~size:1024 ()

let addr ptr =
  C.to_voidp ptr |>
  C.raw_address_of_ptr

let is_managed ptr = Hashtbl.mem managed (addr ptr)

let release ptr = Hashtbl.remove managed (addr ptr)

let str_of_ptr ptr = C.string_of (C.ptr C.void) (C.to_voidp ptr)

let expect_managed ptr =
  invalid_argf "Object at %s is not managed by OCaml"
    (str_of_ptr ptr) ()

let size ptr =
  match Hashtbl.find managed (addr ptr) with
  | None -> expect_managed ptr
  | Some arr -> Array.length arr - 1

let read ptr =
  match Hashtbl.find managed (addr ptr) with
  | None -> expect_managed ptr
  | Some arr ->
    String.init (Array.length arr) ~f:(Array.get arr)

let write str =
  let len = String.length str + 1 in
  let arr = Array.make C.char len in
  for i = 0 to len - 2 do
    arr.(i) <- str.[i];
  done;
  arr.(len - 1) <- '\x00';
  let kind = Bigarray.char in
  let barr = C.bigarray_of_array C.array1 kind arr in
  let ptr = C.bigarray_start C.array1 barr in
  Hashtbl.set managed ~key:(addr ptr) ~data:arr;
  ptr

let read_opt ptr =
  if C.is_null ptr then None else Some (read ptr)
let write_opt = function
  | None -> C.from_voidp C.char C.null
  | Some str -> write str

let () = def "strlen" C.(ptr char @-> returning int) size

let t : string C.typ = C.view ~read ~write (C.ptr C.char)
let nullable : string option C.typ =
  C.view (C.ptr C.char) ~read:read_opt ~write:write_opt

let spec = (t,nullable)
