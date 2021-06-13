(** Library providing an implementation of linear temporal logic. *)

(** @author Matthew Kukla *)

(** No next frame in path. *)
exception No_next_frame

(** Formulae *)
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

(** Represent formula as string. *)
val fmla_as_string : expr -> string 

(** Rewrite formula to only use atomic connectives / operators. *)
val to_atomics : expr -> expr 

(** Evaluate formula over path. *)
val eval_fmla : expr -> (string * bool) list list -> bool
