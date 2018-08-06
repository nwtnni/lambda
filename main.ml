let parse s = Parser.main Lexer.token (Lexing.from_string s)

let eval expr = expr
  |> Bruijn.from_lambda
  |> Bruijn.eval
  |> Bruijn.to_lambda

let apply f l =
  List.fold_left (fun a b -> Lambda.App (a, b)) f l

let t = parse "λt f. t"
let f = parse "λt f. f"
let branch = parse "λl t f. l t f"

let conj = apply (parse "λf a b. a b f") [f]
let disj = apply (parse "λt a b. a t b") [t]
