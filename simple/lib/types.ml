open Sexplib.Conv

type 'a span = 'a * Span.t
[@@deriving sexp]

module Int = struct
  type t = int span
  [@@deriving sexp]
end

module Var = struct
  type t = string span
  [@@deriving sexp]
end

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
  | Int of Int.t
  | True
  | False
  | Unit
  | Var of Var.t
  | Let of Var.t * t * t
  | Abs of Var.t * Type.t * t
  | App of t * t
  | If of t * t * t
  | Bin of Bin.t * t * t
  | Uno of Uno.t * t
  | Prod of t * t
  | Pil of t
  | Pir of t
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
  | LAnd
  | LOr
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
