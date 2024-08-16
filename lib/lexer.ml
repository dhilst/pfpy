let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let char = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_']
let id = [%sedlex.regexp? char, Star (char | digit)]
let fqid = [%sedlex.regexp? id, Plus ('.', id)]

open Parser

exception Eof
exception Lexer_error of string

let string_lex_double lexbuf strbuf =
  Buffer.add_char strbuf '"';
  let lexeme = Sedlexing.Utf8.lexeme in
  let rec lex lexbuf strbuf =
    match%sedlex lexbuf with
    | '"' ->
        Buffer.add_char strbuf '"';
        Buffer.contents strbuf
    | Sub (any, '"') ->
        Buffer.add_string strbuf (lexeme lexbuf);
        lex lexbuf strbuf
    | _ ->
        let error =
          lexeme lexbuf |> Format.asprintf "Unexpected character: >%s<"
        in
        raise (Lexer_error error)
  in
  lex lexbuf strbuf

let rec token buf =
  match%sedlex buf with
  | "#", Star (Sub (any, '\n')), ('\n' | eof) -> token buf
  | Plus (Chars " \t\n") -> token buf
  | ";" -> SEMICOLON
  | "->" -> TARROW
  | "<<" | ">>" | "&" | "^" -> BINOP_3 (Sedlexing.Utf8.lexeme buf)
  | "*" | "/" | "%" -> BINOP_2 (Sedlexing.Utf8.lexeme buf)
  | "+" | "-" -> BINOP_1 (Sedlexing.Utf8.lexeme buf)
  | "==" | "!=" | ">=" | "<=" | ">" | "<" -> BINOP_0 (Sedlexing.Utf8.lexeme buf)
  | number ->
      let i = int_of_string (Sedlexing.Utf8.lexeme buf) in
      INT i
  | "true" -> BOOL true
  | "false" -> BOOL false
  | "def" -> DEF
  | "import" -> IMPORT
  | "from" -> FROM
  | "lambda" -> LAMBDA
  | "match" -> MATCH
  | "let" -> LET
  | "in" -> IN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "with" -> WITH
  | "=>" -> ARROW
  | "|" -> PIPE
  | "end" -> END
  | "type" -> TYPE
  | "(" -> LPAR
  | ")" -> RPAR
  | "[" -> LBRACKET
  | "]" -> RBRACKET
  | "{" -> LBRACES
  | "}" -> RBRACES
  | ":" -> COLON
  | "," -> COMMA
  | "=" -> EQ
  | '"' -> STRING (string_lex_double buf (Buffer.create 200))
  | id -> ID (Sedlexing.Utf8.lexeme buf)
  | fqid -> FQID (Sedlexing.Utf8.lexeme buf)
  | eof -> EOF
  | _ ->
      failwith
        (Format.sprintf "Unexpected character '%s'" (Sedlexing.Utf8.lexeme buf))
