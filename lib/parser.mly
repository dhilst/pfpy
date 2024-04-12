%{
open Ast
%}

%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token <string> FQID
%token <string> ID
%token DEF IMPORT FROM TYPE
%token MATCH WITH ARROW PIPE END
%token IF THEN ELSE
%token COMMA COLON SEMICOLON
%token EOF
%token EQ
%token RBRACKET LBRACKET
%token RBRACES LBRACES
%token RPAR LPAR
%nonassoc IF
%nonassoc LPAR LBRACKET
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
  | expr_0 {}
  | type_ {}
;

type_: 
  | TYPE ID brkcommalist(targ)? EQ expr_0 {}
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

%inline brccommalist(x):
  | delimited(LBRACES, separated_list(COMMA, x), RBRACES) {}
;


targ:
  | ID option(pair(COLON, expr_0)) {}

arg:
  | ID COLON expr_1 {}

def:
  | DEF ID brkcommalist(targ)? parcommalist(arg) COLON expr_0 EQ expr_0 {}
;

// @TODO break this in multiple levels
expr_0:
  | match_ {}
  | expr_1 {}
;

expr_1:
  | if_ {}
  | app {}
  | var {}
  /* | set {} */
  /* | tuple {} */
  /* | dict {} */
  | const {}
;

if_:
  | IF expr_1 THEN expr_1 ELSE expr_1 %prec IF {}

match_:
  | MATCH expr_0 WITH match_pat* END {}
;

match_pat:
  | PIPE expr_0 ARROW expr_0 {}
;

/* set: */
/*   | delimited(LBRACES, separated_nonempty_list(COMMA, expr_1), RBRACES) {} */
/* ; */

/* tuple: */
/*   | delimited(LPAR, separated_nonempty_list(COMMA, expr_1), RPAR) {} */
/* ; */

/* dict: */
/*   | brccommalist(separated_pair(expr_1, COLON, expr_1)) {} */
/* ; */

app:
  | expr_1 parcommalist(expr_1) {}
  | expr_1 brkcommalist(expr_1) {}
;

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
