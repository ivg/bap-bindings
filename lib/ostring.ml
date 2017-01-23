(*
   Implementation details.
   =======================


   Value representation
   --------------------

   An OCaml string is copied to a CArray value, which is a
   custom block that contains a pointer to a malloced buffer.
   An explicit null character is added to the end of the array (so
   the CArray is one character longer than the original string).

   The CArray value is managed by the OCaml GC and is preserved
   with the hash table (called managed), that maps pointers
   (virtual memory addresses) to the CArrays.

   A pointer to the mallocated buffer is returned to the C
   side. When the `bap_release` function is called, a pointer
   passed to the function is used to remove a corresponding entry
   from the hashtable. Once it is removed, it should become
   unreachable and the memory will be collected as soon, as the GC
   comes to it.

   Although the type is mostly used to pass string data from OCaml
   to C, it maybe also used for the opposite side. We're not using
   this direction right now, but the Ctypes interfaces requires us
   to provide the implementation for the both sides. For
   simplicity we are requiring that a pointer passed from the
   C-side must be managed by the OString module. A runtime check
   ensures this. If the value is managed, then it will be copied
   to the string. In the general case we can remove this
   constraint and allow the C side to pass a data ownership to our
   side.


   Performance overhead
   --------------------

   Since we can't pass a pointer to data that is managed by OCaml,
   as the data can be moved by the GC, we have no other choices
   other than copying the data from the OCaml arena to the C
   (malloc) arena. So the performance cost is the cost of copying
   the data (plus an allocation of few custom blocks - the managed
   buffer itself, the nativeing pointer, and a few regular blocks,
   including the fat pointer, and an entry in the managed table).

   Although the performance implication will be also linear to the
   size of the buffer, the constant factor can be reduced by using
   Core's bigstrings instead of the CArray. Being bigarrays
   underneath the hood, they also store their data in the malloc
   arena, so we can safely pass a pointer to data. However, the
   copying is implemented more efficiently with the `memcpy`
   function, unlike a pure OCaml for-loop used for the
   CArray.
*)
module Pool = Nativeint.Table
let managed = Pool.create ~size:1024 ()
module Array = C.CArray

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
    String.init (Array.length arr - 1) ~f:(Array.get arr)

let write str =
  let len = String.length str + 1 in
  let arr = Array.make C.char len in
  for i = 0 to len - 2 do
    arr.(i) <- str.[i];
  done;
  arr.(len - 1) <- '\x00';
  let ptr = Array.start arr in
  Hashtbl.add_exn managed ~key:(addr ptr) ~data:arr;
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
