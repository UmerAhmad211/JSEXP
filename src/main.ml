open Sexplib.Sexp

(*
    opam install sexplib yojson
    ocamlfind ocamlopt -o main -linkpkg -package sexplib -package yojson main.ml
 *)

let rec json_to_sexp (json : Yojson.Basic.t) : Sexplib.Sexp.t =
  match json with
  | `Null -> Atom "null"
  | `Bool b -> Atom (string_of_bool b)
  | `Int i -> Atom (string_of_int i)
  | `Float f -> Atom (string_of_float f)
  | `String s -> Atom s
  | `List lst -> List (List.map json_to_sexp lst)
  | `Assoc kvs ->
      List (List.map (fun (k, v) -> List [ Atom k; json_to_sexp v ]) kvs)

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "Usage: ./main <file_name>";
    exit 1);
  let filename = Sys.argv.(1) in
  try
    let json = Yojson.Basic.from_file filename in
    let sexp = json_to_sexp json in
    Sexplib.Sexp.output_hum stdout sexp;
    print_newline ()
  with
  | Sys_error msg -> Printf.eprintf "File error: %s\n" msg
  | Yojson.Json_error msg -> Printf.eprintf "JSON parse error: %s\n" msg
