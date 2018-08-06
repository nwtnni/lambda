module Context = struct
  type t = string list

  module S = Set.Make (String)
  open Lambda

  let from_lambda expr =
    let rec variables = function 
    | Var x       -> S.singleton x
    | Abs (x, e)  -> S.add x (variables e)
    | App (e, e') -> S.union (variables e) (variables e')
    in
    S.fold List.cons (variables expr) []

  let push = List.cons

  let find var =
    let rec find' n = function
    | []                  -> failwith "Unreachable by construction of context"
    | v :: t when v = var -> n
    | _ :: t              -> find' (n + 1) t
    in
    find' 0
end

module Term = struct
  type t =
  | Var of int
  | Abs of string * t
  | App of t * t
end

type t = Term.t * Context.t
