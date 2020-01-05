
(* Variable bindings at each state are represented by a string * bool list.
   A path is represented as a list of binding lists. *)

type expr = 
    | True
    | False
    | Var of string
    | And of expr * expr
    | Or of expr * expr
    | Impl of expr * expr
    | Iff of expr * expr
    | U of expr * expr      (* ``until`` *)
    | R of expr * expr      (* ``release`` *)
    | W of expr * expr      (* ``weak`` *)
    | M of expr * expr      (* ``strong release`` *)
    | Not of expr 
    | X of expr             (* ◯f *)
    | G of expr             (* □f *)
    | F of expr             (* ◊f *)


let rec fmla_to_string f = 
let l = "(" and r = ")" in
    match f with
        | True -> "⊤"
        | False -> "⊥"
        | Var x -> x
        | And(e,f) -> l ^ (fmla_to_string e) ^ " ∧ " ^ (fmla_to_string f) ^ r
        | Or(e,f) -> l ^ (fmla_to_string e) ^ " v " ^ (fmla_to_string f) ^ r
        | Impl(e,f) -> l ^ (fmla_to_string e) ^ " → " ^ (fmla_to_string f) ^ r
        | Iff(e,f) -> l ^ (fmla_to_string e ) ^ " ↔ " ^ (fmla_to_string f) ^ r
        | U(e,f) -> l ^ (fmla_to_string e) ^ ") U (" ^ (fmla_to_string f) ^ r
        | R(e,f) -> l ^ (fmla_to_string e) ^ ") R (" ^ (fmla_to_string f) ^ r
        | W(e,f) -> l ^ (fmla_to_string e) ^ ") W (" ^ (fmla_to_string f) ^ r
        | M(e,f) -> l ^ (fmla_to_string e) ^ ") M (" ^ (fmla_to_string f) ^ r
        | Not(e) -> "¬(" ^ (fmla_to_string e) ^ r
        | X(e) -> "X(" ^ (fmla_to_string e) ^ r
        | G(e) -> "G(" ^ (fmla_to_string e) ^ r
        | F(e) -> "F(" ^ (fmla_to_string e) ^ r
