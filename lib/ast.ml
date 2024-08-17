type expr =
  (* Statments *)
  | Import of expr
  | FromImport of { mname : expr; symbols : expr list }
  | Def of {
      fname : string;
      targs : expr list option;
      args : expr list;
      rtype : expr;
      body : expr;
    }
  | TypeDef of { tname : string; targs : expr list option; body : expr }
  | LetMAssignStmt of { op : string; fname : string }
  (* let (let* ) = body *)
  (* Expressions *)
  | Match of { scrutinee : expr; patterns : expr list }
  | If of { cond : expr; then_ : expr; else_ : expr }
  | MatchArm of { pattern : expr; body : expr }
  | Let of { op : string option; name : string; value : expr; body : expr }
  | LetMAssign of { op : string; fname : string; body : expr }
  | BinOp of expr * string * expr
  | Tuple of expr list
  | Set of expr list
  | List of expr list
  | Dict of (expr * expr) list
  | FCall of { f : expr; args : expr list }
  | Index of { f : expr; args : expr list }
  | Lambda of { args : expr list; body : expr }
  | TArg of { aname : string; boundary : expr option }
  | Arg of { aname : string; typ : expr }
  | Int of int
  | String of string
  | Bool of bool
  | FQID of string
  | ID of string
  (* TODO *)
  | Todo of string
[@@deriving show]
