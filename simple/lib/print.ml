module Simple = Types

let format_option f fmt o =
  match o with
  | None   -> ()
  | Some e -> f fmt e

module Int = struct
  let format_t fmt e =
    Format.fprintf fmt "%i" (fst e)
end

module Var = struct
  let format_t fmt e =
    Format.fprintf fmt "%s" (fst e)
end

module rec Exp : sig
  val format_t : Format.formatter -> Simple.Exp.t -> unit
end = struct
  open Simple.Exp
  let rec format_t fmt e =
    match fst e with
    | Int n -> Int.format_t fmt n
    | True  -> Format.fprintf fmt "true"
    | False -> Format.fprintf fmt "false"
    | Unit  -> Format.fprintf fmt "()"
    | Var v -> Var.format_t fmt v
    | Let (v, t, e, e') ->
      Format.fprintf fmt "@[<2>let@ %a%a =@ %a@ in@ %a@]"
        Var.format_t v    
        (format_option (fun fmt e -> Format.fprintf fmt ":@ %a " Type.format_t e)) t
        format_t e
        format_t e'
    | _ -> failwith "Unimplemented"
end

and Bin : sig
  val format_t : Format.formatter -> Simple.Bin.t -> unit
end = struct
  open Simple.Bin
  let format_t fmt e =
    let op = match fst e with
    | Add  -> "+"
    | Sub  -> "-"
    | Mul  -> "*"
    | Div  -> "/"
    | LAnd -> "/\\"
    | LOr  -> "\\/"
    | Lt   -> "<"
    | Le   -> "<="
    | Ge   -> ">="
    | Gt   -> ">"
    | Eq   -> "="
    | Ne   -> "!="
    in Format.fprintf fmt "%s" op
end

and Uno : sig
  val format_t : Format.formatter -> Simple.Uno.t -> unit
end = struct
  open Simple.Uno
  let format_t fmt e =
    let op = match fst e with
    | Neg -> "-"
    | Not -> "not "
    in Format.fprintf fmt "%s" op
end

and Type : sig
  val format_t : Format.formatter -> Simple.Type.t -> unit
end = struct
  open Simple.Type
  let format_t fmt e =
    match fst e with
    | Int -> Format.fprintf fmt "int"
    | Unit -> Format.fprintf fmt "unit"
    | Bool -> Format.fprintf fmt "bool"
    | _ -> failwith "unimplemented"
end
