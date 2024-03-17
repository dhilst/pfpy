type op =
  | Add | Mul | Div | Sub
[@@deriving show]

type expr =
  | Id of string
  | Int of int
  | BinOp of op * expr * expr
  | Var of string
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Bool of bool
[@@deriving show]
