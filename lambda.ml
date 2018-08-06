module Map = Map.Make (String)
module Set = Set.Make (String)

type t =
| Var of string
| Abs of string * t
| App of t * t

let rec free = function
| Var x       -> Set.singleton x
| Abs (x, e)  -> Set.remove x (free e)
| App (e, e') -> Set.union (free e) (free e')
