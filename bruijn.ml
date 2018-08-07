module Context = struct
  type t = string list

  let from_lambda expr =
    Lambda.Set.fold List.cons (Lambda.free expr) []

  let push name context =
    name :: context

  let unique name context =
    let regex = Str.regexp ("^" ^ name ^ "'*$") in

    let names = context
    |> List.filter (fun name -> Str.string_match regex name 0)
    |> List.sort (fun a b -> (String.length b) - (String.length a))
    in

    match names with
    | []        -> name
    | name :: _ -> name ^ "'"

  let name index context =
    List.nth context index

  let index name =
    let rec index' n = function
    | []                   -> failwith "Internal error: invalid naming context"
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

let rec eq a b = match a, b with
| Var x, Var y                 -> x = y
| Abs (_, e), Abs (_, e')      -> eq e e'
| App (e, e'), App (e'', e''') -> eq e e'' && eq e' e'''
| _                            -> false

let from_lambda expr =
  let rec from_lambda' context (expr: Lambda.t) = match expr with
  | Var x       -> Var (Context.index x context)
  | Abs (x, e)  -> Abs (x, from_lambda' (Context.push x context) e)
  | App (e, e') -> App (from_lambda' context e, from_lambda' context e')
  in
  let context = Context.from_lambda expr in
  (from_lambda' context expr, context)

let to_lambda (expr, context) =
  let rec to_lambda' context expr : Lambda.t = match expr with
  | Var x       -> Var (Context.name x context)
  | Abs (x, e)  -> let x' = Context.unique x context in
                   Abs (x', to_lambda' (Context.push x' context) e)
  | App (e, e') -> App (to_lambda' context e, to_lambda' context e')
  in
  to_lambda' context expr

let show (expr, _) =
  let rec show' = function
  | Var x                       -> string_of_int x
  | Abs (_, e)                  -> Printf.sprintf "λ. %s"     (show' e)
  | App (e, (App (_, _) as e'))
  | App (e, (Abs (_, _) as e')) -> Printf.sprintf "%s (%s)"   (show' e) (show' e')
  | App (e, e')                 -> Printf.sprintf "%s %s"     (show' e) (show' e')
  in
  show' expr

let walk cond map =
  let rec walk' d = function 
  | Var x when cond x d -> map x d
  | Var x               -> Var x
  | Abs (x, e)          -> Abs (x, walk' (d + 1) e)
  | App (e, e')         -> App (walk' d e, walk' d e')
  in
  walk' 0

let shift d =
  walk (fun x cutoff -> x >= cutoff)
       (fun x _      -> Var (x + d))

let substitute var expr =
  walk (fun x depth -> x = var + depth)
       (fun _ depth -> shift depth expr)

let rec eval (expr, context) =
  let rec eval' = function 
  | App (Abs (x, e), e') -> shift (-1) (substitute 0 (shift 1 e') e)
  | App (e, e') -> App (eval' e, eval' e')
  | Abs (x, e) -> Abs (x, eval' e)
  | e -> e
  in
  let expr' = eval' expr in
  if eq expr expr'
  then (expr', context)
  else eval (expr', context)
