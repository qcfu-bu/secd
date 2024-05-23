type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Var of string
  | Fun of string * string * expr
  | Ifte  of expr * expr * expr
  | Let of string * expr * expr
  | App of expr * expr
[@@deriving show {with_path = false}]
