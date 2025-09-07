open Json_val

type sexp =
  | Atom of string
  | List of sexp list

let rec sexp_of_json = function
  | Int i -> Atom (string_of_int i)
  | Float f -> Atom (string_of_float f)
  | String s -> Atom ("\"" ^ s ^ "\"")
  | Boolean true -> Atom "true"
  | Boolean false -> Atom "false"
  | Null -> Atom "null"
  | List l -> List (List.map sexp_of_json l)
  | Assoc fields ->
      List (List.map (fun (k, v) -> List [ Atom k; sexp_of_json v ]) fields)

let rec string_of_sexp = function
  | Atom a -> a
  | List l -> "(" ^ String.concat " " (List.map string_of_sexp l) ^ ")"
