module Map = Map.Make (String)
module Set = Set.Make (String)

type context = t Map.t

and t =
| Var of string
| Abs of string * t
| App of t * t
