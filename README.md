# lambda

This library defines a named and unnamed representation, an
accompanying lexer and parser, and a small standard library of booleans, numerals,
and functions implemented in the lambda calculus itself.

Currently uses the named representation for display and input,
and the unnamed [de Bruijn indices][1] for reduction.

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

Implementing the [Z-combinator][2]:

```ocaml
let z = parse "λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))"
(* val z : Lambda.t = λf. (λx. f (λy. x x y)) (λx. f (λy. x x y)) *);;

Bruijn.from_lambda z
(* - : Bruijn.term * string list = λ. (λ. 1 (λ. 1 1 0)) (λ. 1 (λ. 1 1 0)) *);;
```

Performing arithmetic with [Church numerals][3]:

```ocaml
let n = apply mul [church 3; church 2] |> eval
(* val n : Lambda.t = λs. λz. s (s (s (s (s (s z))))) *);;
```

[1]: https://en.wikipedia.org/wiki/De_Bruijn_index
[2]: https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator
[3]: https://en.wikipedia.org/wiki/Church_encoding
