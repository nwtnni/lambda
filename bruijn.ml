module Context = struct
  type t = string list

  let from_lambda expr =
    Lambda.Set.fold List.cons (Lambda.free expr) []

  let push name context =
    name :: context

  let name index context =
    List.nth context index

  let index name =
    let rec index' n = function
    | []                   -> failwith "Unreachable by construction of context"
    | x :: t when x = name -> n
    | _ :: t               -> index' (n + 1) t
    in
  index' 0
end

type term =
| Var of int
| Abs of string * term
| App of term * term

type t = term * Context.t

let from_lambda expr =
  let rec from_lambda' context (expr: Lambda.t) = match expr with
  | Var x       -> Var (Context.index x context)
  | Abs (x, e)  -> Abs (x, from_lambda' (Context.push x context) e)
  | App (e, e') -> App (from_lambda' context e, from_lambda' context e')
  in
  let context = Context.from_lambda expr in
  (from_lambda' context expr, context)
