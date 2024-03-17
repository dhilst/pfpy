%{
open Ast
%}

%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token DEF IMPORT FROM TYPE
%token MATCH WITH ARROW PIPE END IF THEN ELSE
%token COMMA COLON SEMICOLON
%token EOF
%token EQ
%token RPAR LPAR
%token RBRACKET LBRACKET
%token <string> FQID
%token <string> ID
%start main             /* the entry point */
%type <expr> main
%%

main:
 | list(stmts) EOF { Int 0 };
;

stmts:
 | stmt SEMICOLON {}
;

stmt:
  | import  {}
  | def {}
  | expr {}
  | type_ {}
;

type_: 
  | TYPE ID brkcommalist(arg)? EQ expr {}
;

fqid:
  | FQID {}
  | ID {}
;

import:
  | IMPORT fqid {}
  | FROM fqid IMPORT separated_list(COMMA, fqid) {}
;

%inline parcommalist(x):
  | delimited(LPAR, separated_list(COMMA, x), RPAR) {}
;

%inline brkcommalist(x):
  | delimited(LBRACKET, separated_list(COMMA, x), RBRACKET) {}
;

arg:
  | ID option(pair(COLON, expr)) {}

def:
  | DEF ID brkcommalist(arg)? parcommalist(arg) COLON expr EQ expr {}
;

// @TODO break this in multiple levels
expr:
  | match_ {}
  | if_ {}
  | app {}
  | var {}
  | const {}
;

if_:
  | IF expr THEN expr ELSE expr {}

match_:
  | MATCH expr WITH match_pat* END {}
;

match_pat:
  | PIPE expr ARROW expr {}
;

app:
  | expr parcommalist(expr) {}
  | expr brkcommalist(expr) {}

var:
  | FQID {}
  | ID {}
;

const:
  | STRING {} 
  | INT {} 
  | BOOL {}
;
%%
