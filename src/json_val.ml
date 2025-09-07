type value =
  | Assoc of (string * value) list
  | Int of int
  | Float of float
  | String of string
  | Boolean of bool
  | List of value list
  | Null
