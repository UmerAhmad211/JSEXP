open Sexp_gen
open Util

let usage_msg = "jsexp <in_file_name> -o <out_file_name>"
let in_file_name = ref ""
let out_file_name = ref ""
let speclist = [ ("-o", Arg.Set_string out_file_name, "Set output file name") ]

let anon_fun filename =
  if !in_file_name = "" then
    in_file_name := filename
  else
    print_err_usage_fail ~usage:usage_msg "Error: Only one file expected."

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !in_file_name = "" && check_filename_exe !in_file_name then
    print_err_usage_fail ~usage:usage_msg "Error: Input file error.";

  let ic = open_in !in_file_name in
  let lexbuf = Lexing.from_channel ic in

  let json =
    try Parser.program Lexer.read_token lexbuf
    with Parser.Error ->
      pprint_error !in_file_name lexbuf;
      exit 1
  in
  close_in ic;

  let sexp = sexp_of_json json in
  let out =
    if !out_file_name = "" then
      stdout
    else
      open_out !out_file_name
  in
  output_string out (string_of_sexp sexp ^ "\n");
  if out != stdout then
    close_out out
