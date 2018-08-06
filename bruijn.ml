type t =
| Var of int
| Abs of string * t
| App of t * t
