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

let pair = parse "λf s b. b f s"
let first = apply (parse "λt p. p t") [t]
let second = apply (parse "λf p. p f") [f]

let zero = parse "λs z. z"
let succ = parse "λn s z. s (n s z)"

let rec church n = match n with
| 0 -> zero
| n -> apply succ [church (n - 1)]

let add = parse "λm n s z. m s (n s z)"
let mul = apply (parse "λadd zero m n. m (add n) zero") [add; zero]
let exp = apply (parse "λmul one m n. n (mul m) one") [mul; church 1]

let is_zero = apply (parse "λt f n. n (λanything. f) t") [t; f]

let zz = apply (parse "λpair zero. pair zero zero") [pair; zero]
let ss = apply (parse "λpair second add one p. pair (second p) (add one (second p))") [pair; second; add; church 1]
let pred = apply (parse "λfirst ss zz n. first (n ss zz)") [first; ss; zz]
