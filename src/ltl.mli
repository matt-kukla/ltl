(** Linear temporal logic *)

(** @author Matthew Kukla *)

(** No next frame in path. *)
exception No_next_frame

type expr =
    True
    | False
    | Var of string
    | And of expr * expr
    | Or of expr * expr
    | Impl of expr * expr
    | Iff of expr * expr
    | U of expr * expr (** until *)
    | R of expr * expr (** release *)
    | W of expr * expr (** weak *)
    | M of expr * expr (** strong release *)
    | Not of expr
    | X of expr (** ◯f *)
    | G of expr (** □f *)
    | F of expr (** ◊f *)

(** Represent formula as string. *)
val fmla_as_string : expr -> string 

(** Rewrite formula to use only atomic connectives/modal operators. *)
val to_atomics : expr -> expr 

(** Evaluate formula over path. *)
val eval_fmla : expr -> (string * bool) list list -> bool

(** Calculate maximum depth of nested modalities in a formula. *)
val modal_depth : expr -> int
