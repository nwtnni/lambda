open Bruijn
open Lambda

let parse s = Parser.main Lexer.token (Lexing.from_string s)
