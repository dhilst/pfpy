%{
open Ast
%}

%token <int> INT
%token <string> STRING
%token <string> BINOP_0
%token <string> BINOP_1
%token <string> BINOP_2
%token <string> BINOP_3
%token <bool> BOOL
%token <string> FQID
%token <string> ID
%token TARROW
%token LAMBDA
%token DEF IMPORT FROM TYPE
%token MATCH WITH ARROW PIPE END
%token LET IN
%token IF THEN ELSE
%token COMMA COLON SEMICOLON
%token EOF
%token EQ
%token RBRACKET LBRACKET
%token RBRACES LBRACES
%token RPAR LPAR
%nonassoc IF
%nonassoc BINOP_3
%left BINOP_2
%left BINOP_1
%right TARROW
%nonassoc BINOP_0
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
  | ID option(pair(COLON, expr_1)) {}

arg:
  | ID COLON expr_1 {}

def:
  | DEF ID brkcommalist(targ)? parcommalist(arg) COLON expr_0 EQ expr_0 {}
;

// @TODO break this in multiple levels
expr_0:
  | match_ {}
  | let_ {}
  | expr_1 {}
;

expr_1:
  | bin_expr {}
  | lambda {}
  | if_ {}
  | app {}
  | var {}
  | tuple {}
  | set {}
  | list_ {}
  | dict {}
  | const {}
  | LPAR expr_1 RPAR {}
;

bin_expr: 
  | expr_1 BINOP_0 expr_1 %prec BINOP_0 {}
  | expr_1 BINOP_1 expr_1 %prec BINOP_1 {}
  | expr_1 BINOP_2 expr_1 %prec BINOP_2 {}
  | expr_1 BINOP_3 expr_1 %prec BINOP_3 {}
  | expr_1 TARROW expr_1 %prec TARROW {}
;

lambda:
  | LAMBDA parcommalist(arg) ARROW expr_1 %prec IF {}
;

let_: 
  | LET ID EQ expr_1 IN expr_0 {}
;

if_:
  | IF expr_1 THEN expr_1 ELSE expr_1 %prec IF {}
;

match_:
  | MATCH expr_0 WITH match_pat* END {}
;

match_pat:
  | PIPE expr_0 ARROW expr_0 {}
;

list_:
  | delimited(LBRACKET, separated_nonempty_list(COMMA, expr_1), RBRACKET) {}
;

set:
  | delimited(LBRACES, separated_nonempty_list(COMMA, expr_1), RBRACES) {}
;

tuple:
  | LPAR expr_1 COMMA separated_list(COMMA, expr_1) RPAR {}
;

dict:
  | brccommalist(separated_pair(expr_1, COLON, expr_1)) {}
;

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
