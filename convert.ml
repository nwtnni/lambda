let naming_context (expr: Lambda.t) : string list =
  let module S = Set.Make (String) in
  let rec variables (expr: Lambda.t) = match expr with
  | Var x       -> S.singleton x
  | Abs (x, e)  -> S.add x (variables e)
  | App (e, e') -> S.union (variables e) (variables e')
  in
  S.fold List.cons (variables expr) []
