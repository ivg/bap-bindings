open Core_kernel.Std


let current_error : Error.t option ref = ref None

let set err = current_error := Some err
let clear () = current_error := None

let failf fmt = Format.kasprintf (fun str ->
    let err = Error.of_string str in
    set err;
    Error err) fmt

let lift x = match x with
  | Ok x -> Some x
  | Error err -> set err; None

let lift1 f x = match f x with
  | Ok x -> Some x
  | Error err -> set err; None

let lift2 f x y = match f x y with
  | Ok x -> Some x
  | Error err -> set err; None
