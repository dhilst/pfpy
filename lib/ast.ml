[@@@ocaml.warning "-26-27"]

open Format

type expr =
  (* Statments *)
  | Module of expr list
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
  | DataTypeDef of { tname : string; targs : expr list option; body : expr }
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

let indent n s = sprintf "%s%s" (String.make (n * 4) ' ') s

let rec to_py ?(depth = 0) : expr -> string = function
  | Module exprs -> String.concat "\n\n" @@ List.map to_py exprs
  | Import expr -> sprintf "import %s" @@ to_py expr
  | FromImport { mname; symbols } ->
      sprintf "from %s import %s" (to_py mname)
        (List.map to_py symbols |> String.concat ", ")
  | Def { fname; targs = None; args; rtype; body } ->
      let depth = depth + 1 in
      sprintf "def %s(%s) -> %s:\n%s" fname
        (List.map to_py args |> String.concat ", ")
        (to_py rtype)
        (insert_return ~depth body |> indent depth)
  | Def { fname; targs = Some targs; args; rtype; body } ->
      let depth = depth + 1 in
      sprintf "def %s[%s](%s) -> %s:\n%s" fname
        (List.map to_py targs |> String.concat ", ")
        (List.map to_py args |> String.concat ", ")
        (to_py rtype)
        (insert_return ~depth body |> indent depth)
  | DataTypeDef { tname; targs = None; body } ->
      let dataclasses = gendataclasses body in
      sprintf "%s\ntype %s = %s" dataclasses tname (to_py body)
  | DataTypeDef { tname; targs = Some targs; body } ->
      let dataclasses = gendataclasses body in
      let typs = List.map to_py targs |> String.concat ", " in
      sprintf "%s\ntype %s[%s] = %s" dataclasses tname typs (to_py body)
  | TypeDef { tname; targs = None; body } ->
      sprintf "type %s = %s" tname (to_py body)
  | TypeDef { tname; targs = Some targs; body } ->
      sprintf "type %s[%s] = %s" tname
        (List.map to_py targs |> String.concat ", ")
        (to_py body)
  | LetMAssignStmt { op; fname } -> "# todo let assign stmt ..."
  | Match { scrutinee; patterns } ->
      let depth = depth + 1 in
      sprintf "match %s:\n%s" (to_py scrutinee)
        (List.map (fun expr -> to_py ~depth expr) patterns |> String.concat "\n")
  | MatchArm { pattern; body } ->
      sprintf "case %s:\n%s" (to_py pattern)
        (to_py ~depth:(depth + 1) body |> indent (depth + 1))
      |> indent depth
  | If { cond; then_; else_ } ->
      sprintf "%s if %s else %s" (to_py then_) (to_py cond) (to_py else_)
  | Let { op = None; name; value; body } ->
      sprintf "%s = %s;\n%s" name (to_py value) (to_py body)
  | Let { op = Some op; name; value; body } -> "# todo monadic let ..."
  | LetMAssign { op; fname; body } -> "# todo monadic let assign ..."
  | BinOp (e1, "->", e2) -> sprintf "Callable[[%s], %s]" (to_py e1) (to_py e2)
  | BinOp (e1, op, e2) -> sprintf "%s %s %s" (to_py e1) op (to_py e2)
  | Tuple exprs -> sprintf "(%s)" @@ String.concat ", " @@ List.map to_py exprs
  | Set exprs -> sprintf "{%s}" @@ String.concat ", " @@ List.map to_py exprs
  | List exprs -> sprintf "[%s]" @@ String.concat ", " @@ List.map to_py exprs
  | Dict pairs ->
      sprintf "{%s}" @@ String.concat ", "
      @@ List.map (fun (k, v) -> sprintf "%s: %s" (to_py k) (to_py v)) pairs
  | FCall { f; args } ->
      sprintf "%s(%s)" (to_py f) (List.map to_py args |> String.concat ", ")
  | Index { f; args } ->
      sprintf "%s[%s]" (to_py f) (List.map to_py args |> String.concat ", ")
  | Lambda { args; body } ->
      let args =
        List.map
          (function Arg { aname; _ } -> aname | _ -> failwith "bad ast")
          args
      in
      sprintf "(lambda %s: %s)" (String.concat ", " args) (to_py body)
  | TArg { aname; boundary = None } -> aname
  | TArg { aname; boundary = Some boundary } ->
      sprintf "%s: %s" aname (to_py boundary)
  | Arg { aname; typ } -> sprintf "%s: %s" aname (to_py typ)
  | Int int -> sprintf "%d" int
  | String string -> string
  | Bool bool ->
      if bool then
        "True"
      else
        "False"
  | FQID id -> id
  | ID id -> id
  | Todo s -> failwith @@ sprintf "Todo found %s" s

and insert_return ?(depth = 0) = function
  | Match { scrutinee; patterns } ->
      let depth = depth + 1 in
      sprintf "match %s:\n%s" (to_py scrutinee)
        (List.map (fun expr -> insert_return ~depth expr) patterns
        |> String.concat "\n")
  | MatchArm { pattern; body } ->
      sprintf "case %s:\n%s" (to_py pattern)
        (insert_return ~depth:(depth + 1) body |> indent (depth + 1))
      |> indent depth
  | expr -> sprintf "return %s" (to_py ~depth expr)

and gendataclass = function
  | Index { f = ID name; args } ->
      let typs =
        List.map to_py
          (* (function Arg { typ; _ } -> to_py typ | _ -> assert false) *)
          args
        |> String.concat ", "
      in
      let args =
        List.mapi
          (fun index expr ->
            to_py expr |> sprintf "value%d: %s" index |> indent 1)
          args
        |> String.concat "\n"
      in
      sprintf {|@dataclass
class %s[%s]:
%s
|} name typs args
  | e -> failwith @@ sprintf "Bad data type definition %s" (show_expr e)

and gendataclasses = function
  | Index { f = ID "Union"; args } ->
      let ds = List.map gendataclass args |> String.concat "\n" in
      ds
  | e -> failwith @@ sprintf "Bad data type definition %s" (show_expr e)
