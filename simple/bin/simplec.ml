open Simple

let () =
  let file = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel file in
  let _ = Parse.program Lex.token lexbuf in
  ()
