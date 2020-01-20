type expr =
    True
  | False
  | Var of string
  | And of expr * expr
  | Or of expr * expr
  | Impl of expr * expr
  | Iff of expr * expr
  | U of expr * expr
  | R of expr * expr
  | W of expr * expr
  | M of expr * expr
  | Not of expr
  | X of expr
  | G of expr
  | F of expr
val fmla_as_string : expr -> string
val to_atomics : expr -> expr
