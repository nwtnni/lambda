module rec Type : sig
  type t =
  | Int
end = struct
  include Type
end

and Exp : sig 
  type t =
  | Var of string
  | Abs of string * Type.t * t
  | App of t * t
  | Int of int
end = struct
  include Exp
end
