{
open Parser
open Lexing
exception Syntax_Error of string

let next_line lexbuf = 
    let pos = lexbuf.lex_curr_p in 
    lexbuf.lex_curr_p <- {
        pos with pos_bol = lexbuf.lex_curr_pos;
        pos_lnum = pos.pos_lnum + 1
    }
}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z' ]
let int = '-'? digit+
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let ident = ( alpha ) ( alpha|digit|'_' )*
let whitespace = [ ' ' '\t' ]+
let newline = '\r' | '\n' | "\r\n"

rule read_token = parse
    | whitespace    { read_token lexbuf }
    | newline       { next_line lexbuf; read_token lexbuf }
    | int           { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float         { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | "true"        { TRUE }
    | "false"       { FALSE }
    | "null"        { NULL }
    | '"'           { read_string (Buffer.create 20) lexbuf }
    | '{'           { LBRACE }
    | '}'           { RBRACE }
    | '['           { LBRACK }
    | ']'           { RBRACK }
    | ':'           { COLON }
    | ','           { COMMA }
    | eof           { EOF }
    | _             { raise (Syntax_Error ("Error: Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
    | '"'           { STRING (Buffer.contents buf) }
    | '\\' '/'      { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'      { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'      { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
    | _             { raise (Syntax_Error ("Error: Illegal character: " ^ Lexing.lexeme lexbuf)) }
    | eof           { raise (Syntax_Error ("Error: String terminate error.")) }
