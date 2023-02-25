(** Linear temporal logic *)

(** @author Matthew Kukla *)

(** No next state in path. *)
exception No_next_state

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
    | X of expr (** ◯f -- true in next state *)
    | G of expr (** □f -- globally true *)
    | F of expr (** ◊f -- eventually true *)

(** Convert formula to negation normal form.  Result contains only 
propositional connectives and U, R, X.  Negations will appear in front of atomic symbols.*)
val to_nnf : expr -> expr

(** Formula as string *)
val fmla_as_string : expr -> string 

(** Rewrite formula to use only propositional operators, X, and U. *)
val to_atomics : expr -> expr 

(** Evaluate formula over path. *)
val eval_fmla : expr -> (string * bool) list list -> bool

(** Maximum depth of nested modalities in a formula. *)
val modal_depth : expr -> int
