module Map = Map.Make (String)

let parse s = Parser.main Lexer.token (Lexing.from_string s)

let rec step expr = expr
|> Bruijn.from_lambda
|> Bruijn.step
|> Bruijn.to_lambda

let eval expr = expr
|> Bruijn.from_lambda
|> Bruijn.eval
|> Bruijn.to_lambda

let apply f l =
  List.fold_left (fun a b -> Lambda.App (a, b)) f l

let t = parse "λt f. t"
let f = parse "λt f. f" let branch = parse "λl t f. l t f"

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

let add = apply (parse "λsucc m n. m succ n") [succ]
let mul = apply (parse "λadd zero m n. m (add n) zero") [add; zero]
let exp = apply (parse "λmul one m n. n (mul m) one") [mul; church 1]

let is_zero = apply (parse "λt f n. n (λanything. f) t") [t; f]

let zz = apply (parse "λpair zero. pair zero zero") [pair; zero]
let ss = apply (parse "λpair second add one p. pair (second p) (add one (second p))") [pair; second; add; church 1]
let pred = apply (parse "λfirst ss zz n. first (n ss zz)") [first; ss; zz]

let sub = apply (parse "λpred m n. n pred m") [pred]

let fix = parse "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))"

let fact' = apply
  (parse "λbranch is_zero one mul pred f n. branch (is_zero n) one (mul n (f (pred n)))")
  [branch; is_zero; church 1; mul; pred]
let fact = apply fix [fact']

let env = [
    ("t", t);
    ("f", f);
    ("branch", branch);
    ("conj", conj);
    ("disj", disj);
    ("pair", pair);
    ("first", first);
    ("second", second);
    ("zero", zero);
    ("succ", succ);
    ("add", add);
    ("mul", mul);
    ("exp", exp);
    ("is_zero", is_zero);
    ("pred", pred);
    ("sub", sub);
    ("fix", fix);
    ("fact", fact);
  ]
  |> List.to_seq
  |> Map.of_seq
