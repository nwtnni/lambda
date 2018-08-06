open Lambda
module B = Bruijn
module L = Lambda

let parse s = Parser.main Lexer.token (Lexing.from_string s)
