%{
open Json_val
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token COLON
%token COMMA
%token EOF

%start program
%type <Json_val.value> program
%type <Json_val.value> value

%%

let program := 
    | v = value; EOF; { v }

let value := 
    | LBRACE; obj = obj_fields; RBRACE;         { Assoc obj }
    | LBRACK; val_list = list_fields; RBRACK;   { List val_list }
    | s = STRING;                               { String s }
    | i = INT;                                  { Int i }
    | f = FLOAT;                                { Float f }
    | TRUE;                                     { Boolean true }
    | FALSE;                                    { Boolean false }
    | NULL;                                     { Null }

let obj_fields := 
    obj  = separated_list(COMMA, obj_field);    { obj }

let obj_field == 
    o = STRING; COLON; v = value;               { (o,v) }

let list_fields == 
    vl = separated_list(COMMA, value);          { vl }
