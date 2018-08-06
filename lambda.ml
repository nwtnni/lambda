module Map = Map.Make (String)
module Set = Set.Make (String)

type t =
| Var of string
| Abs of string * t
| App of t * t
