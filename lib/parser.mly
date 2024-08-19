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
%token DATA
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
  | list(stmts) EOF { Module $1 };
;

stmts:
 | stmt SEMICOLON { $1 }
;

stmt:
  | import  { $1 }
  | def { $1 }
  | expr_0 { $1 }
  | type_ { $1 }
  | data { $1 }
;

type_: 
  | TYPE ID brkcommalist(targ)? EQ expr_0 { TypeDef { tname = $2; targs = $3; body = $5 } }
;

data: 
  | DATA ID brkcommalist(targ)? EQ expr_0 { DataTypeDef { tname = $2; targs = $3; body = $5 } }
;

fqid:
  | FQID { FQID $1 }
  | ID { ID $1 }
;

import:
  | IMPORT fqid { Import $2 }
  | FROM fqid IMPORT separated_list(COMMA, fqid) { FromImport { mname = $2; symbols = $4 } }
;

%inline parcommalist(x):
  | delimited(LPAR, separated_list(COMMA, x), RPAR) { $1 }
;

%inline brkcommalist(x):
  | delimited(LBRACKET, separated_list(COMMA, x), RBRACKET) { $1 }
;

%inline brccommalist(x):
  | delimited(LBRACES, separated_list(COMMA, x), RBRACES) { $1 }
;

targ:
  | ID option(pair(COLON, expr_1)) { TArg { aname = $1; boundary = Option.map snd $2 } }
;

arg:
  | ID COLON expr_1 { Arg { aname = $1; typ = $3 } }
;

def:
  | DEF ID brkcommalist(targ)? parcommalist(arg) COLON expr_0 EQ expr_0 { Def { fname = $2; targs = $3; args = $4; rtype = $6; body = $8 } }
;

// @TODO break this in multiple levels
expr_0:
  | match_ { $1 }
  | let_{ $1 }
  | expr_1 { $1 }
;

expr_1:
  | bin_expr { $1 }
  | lambda { $1 }
  | if_ { $1 }
  | app { $1 }
  | var { $1 }
  | tuple { $1 }
  | set { $1 }
  | list_ { $1 }
  | dict { $1 }
  | const { $1 }
  | LPAR expr_1 RPAR { $2 }
;

bin_expr: 
  | expr_1 BINOP_0 expr_1 %prec BINOP_0 { BinOp ($1, $2, $3) }
  | expr_1 BINOP_1 expr_1 %prec BINOP_1 { BinOp ($1, $2, $3) }
  | expr_1 BINOP_2 expr_1 %prec BINOP_2 { BinOp ($1, $2, $3) }
  | expr_1 BINOP_3 expr_1 %prec BINOP_3 { BinOp ($1, $2, $3) }
  | expr_1 TARROW expr_1 %prec TARROW { BinOp ($1, "->", $3) }
;

lambda:
  | LAMBDA parcommalist(arg) ARROW expr_1 %prec IF { Lambda { args = $2; body = $4 } }
;

let_: 
  | LET ID EQ expr_0 IN expr_0 {  Let { pat = ID $2; value = $4; body = $6 } }
  | LET tuple EQ expr_0 IN expr_0 {  Let { pat = $2; value = $4; body = $6 } }
;

if_:
  | IF expr_1 THEN expr_1 ELSE expr_1 %prec IF { If { cond = $2; then_ = $4; else_ = $6 } }
;

match_:
  | MATCH expr_0 WITH match_pat* END { Match { scrutinee = $2; patterns = $4 } }
;

match_pat:
  | PIPE expr_0 ARROW expr_0 { MatchArm { pattern = $2; body = $4 } }
;

list_:
  | delimited(LBRACKET, separated_nonempty_list(COMMA, expr_1), RBRACKET) { List $1 }
;

set:
  | delimited(LBRACES, separated_nonempty_list(COMMA, expr_1), RBRACES) { Set $1 }
;

tuple:
  | LPAR expr_1 COMMA separated_list(COMMA, expr_1) RPAR { Tuple ($2 :: $4) }
;

dict:
  | brccommalist(separated_pair(expr_1, COLON, expr_1)) { Dict $1 }
;

app:
  | expr_1 parcommalist(expr_1) { FCall { f = $1; args = $2 } }
  | expr_1 brkcommalist(expr_1) { Index { f = $1; args = $2 } }
;

var:
  | FQID { FQID $1 }
  | ID { ID $1 }
;

const:
  | STRING { String $1 } 
  | INT { Int $1 } 
  | BOOL { Bool $1 }
;
%%
