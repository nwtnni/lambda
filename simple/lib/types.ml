type 'a span = 'a * Span.t
[@@deriving sexp]

module rec Type : sig
  type pre_t =
  | Int
  | Bool
  | Unit
  | Fun of t * t
  | Prod of t * t
  | Sum of t * t
  [@@deriving sexp]

  and t = pre_t span
  [@@deriving sexp]
end = struct
  include Type
end

and Exp : sig 
  type pre_t =
  | Int of int
  | True
  | False
  | Unit
  | Var of string
  | Abs of string * Type.t * t
  | App of t * t
  | Bin of Bin.t * t * t
  | Uno of Uno.t * t
  | Prod of t * t
  | Proj of t * int
  | Inl of Type.t * t
  | Inr of Type.t * t
  | Case of t * t * t
  [@@deriving sexp]

  and t = pre_t span
  [@@deriving sexp]
end = struct
  include Exp
end

and Bin : sig
  type pre_t =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Lt
  | Le
  | Ge
  | Gt
  | Eq
  | Ne
  [@@deriving sexp]

  type t = pre_t span
  [@@deriving sexp]
end = struct
  include Bin
end

and Uno : sig
  type pre_t =
  | Neg
  | Not
  [@@deriving sexp]

  type t = pre_t span
  [@@deriving sexp]
end = struct
  include Uno
end
