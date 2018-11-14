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

let rec show = function
| Var x                                     -> x
| Abs (x, e)                                -> Printf.sprintf "λ%s. %s" x (show e)
| App (Abs (_, _) as e, (Abs (_, _) as e')) -> Printf.sprintf "(%s) (%s)" (show e) (show e')
| App (Abs (_, _) as e, e')                 -> Printf.sprintf "(%s) %s"   (show e) (show e')
| App (e, (App (_, _) as e')) 
| App (e, (Abs (_, _) as e'))               -> Printf.sprintf "%s (%s)"   (show e) (show e')
| App (e, e')                               -> Printf.sprintf "%s %s"     (show e) (show e')
