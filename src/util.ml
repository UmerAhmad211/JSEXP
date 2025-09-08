open Lexing
open Printf

let print_err_usage_fail ?(usage = "") error_msg =
  prerr_endline error_msg;
  prerr_endline usage;
  exit 1

let check_filename_exe filename = Filename.extension filename = ".json"

let pprint_error file_name lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line_num = pos.pos_lnum in
  let col_num = pos.pos_cnum - pos.pos_bol in

  let lines =
    let ic = open_in file_name in
    let rec loop acc =
      try loop (input_line ic :: acc)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    loop []
  in

  let get_line n =
    if n >= 1 && n <= List.length lines then
      List.nth lines (n - 1)
    else
      ""
  in

  let bf = get_line (line_num - 1) in
  let curr = get_line line_num in
  let af = get_line (line_num + 1) in

  let red = "\x1b[31m" in
  let reset = "\x1b[0m" in
  let green = "\x1b[32m" in

  eprintf "%sError:%s Syntax error at %s:%d:%d\n" red reset file_name line_num
    (col_num + 1);

  if bf <> "" then
    eprintf "%s%d%s | %s\n" green (line_num - 1) reset bf;
  eprintf "%s%d%s | %s\n" green line_num reset curr;
  eprintf "%s%s^%s\n" red (String.make col_num '-') reset;
  if af <> "" then
    eprintf "%s%d%s | %s\n" green (line_num + 1) reset af
