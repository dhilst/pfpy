%{
open Ast
%}

%token <int> INT
%token <string> ID
%token LET
%token EQ
%token IN
%token IF
%token THEN
%token ELSE
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%start main             /* the entry point */
%type <expr> main
%%

main:
    INT EOL              { Int $1 }
;
