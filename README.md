# lambda

This library defines basic representations of the untyped lambda calculus, an
accompanying lexer and parser, and a small standard library of booleans, numerals,
and functions implemented in the lambda calculus.

Currently uses a basic named representation for display and input, as well as an
unnamed representation using de Bruijn indices for reduction.

A `.ocamlinit` file is included for ease of exploration in `utop`.

# Examples

All examples are meant to be run in `utop`, after using `make` to build the library.

Parsing the identity combinator:

```ocaml
parse "lambda x. x"
(* - : Lambda.t = λx. x *);;
```

Converting to the de Bruijn representation:

```ocaml
parse "λx. x" |> Bruijn.from_lambda
(* - : Bruijn.term * string list = λ. 0 *);;
```

Z-combinator:

```ocaml
let z = parse "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))"
(* val z : Lambda.t = λf. (λx. f (λy. x x y)) (λx. f (λy. x x y)) *);;

Bruijn.from_lambda z
(* - : Bruijn.term * string list = λ. (λ. 1 (λ. 1 1 0)) (λ. 1 (λ. 1 1 0)) *)
```

Arithmetic with Church numerals:

```ocaml
let n = apply mul [church 3; church 2] |> eval
(* val n : Lambda.t = λs. λz. s (s (s (s (s (s z))))) *);;
```
