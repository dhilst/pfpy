[@@@ocaml.warning "-21"]
open Pfpy

let () =
  try
    let lexbuf = Sedlexing.Utf8.from_channel stdin in
    while true do
      let lexer  = Sedlexing.with_tokenizer Lexer.token lexbuf in
      let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.main in
      let result = parser lexer in
      Format.printf "parsed> %s" (Pfpy.Ast.show_expr result);
      print_newline(); flush stdout;
      exit 0;
      (* let result' = Pfpy.Interpreter.eval result in *)
      (* Format.printf "result> %d" result'; print_newline(); flush stdout *)
    done
  with Lexer.Eof ->
    exit 0
