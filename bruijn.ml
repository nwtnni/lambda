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

type term =
| Var of int
| Abs of string * term
| App of term * term

type t = term * Context.t

let from_lambda expr =
  let rec from_lambda' context (expr: Lambda.t) = match expr with
  | Var x       -> Var (Context.find x context)
  | Abs (x, e)  -> Abs (x, from_lambda' (Context.push x context) e)
  | App (e, e') -> App (from_lambda' context e, from_lambda' context e')
  in
  let context = Context.from_lambda expr in
  (from_lambda' context expr, context)
