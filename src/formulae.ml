
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

(* Represent formula as a string *)
let rec fmla_as_string x = 
let l = "(" and r = ")" in
    match x with
        | True -> "⊤"
        | False -> "⊥"
        | Var x -> x
        | And(e,f) -> l ^ (fmla_as_string e) ^ " ∧ " ^ (fmla_as_string f) ^ r
        | Or(e,f) -> l ^ (fmla_as_string e) ^ " v " ^ (fmla_as_string f) ^ r
        | Impl(e,f) -> l ^ (fmla_as_string e) ^ " → " ^ (fmla_as_string f) ^ r
        | Iff(e,f) -> l ^ (fmla_as_string e ) ^ " ↔ " ^ (fmla_as_string f) ^ r
        | U(e,f) -> l ^ (fmla_as_string e) ^ ") U (" ^ (fmla_as_string f) ^ r
        | R(e,f) -> l ^ (fmla_as_string e) ^ ") R (" ^ (fmla_as_string f) ^ r
        | W(e,f) -> l ^ (fmla_as_string e) ^ ") W (" ^ (fmla_as_string f) ^ r
        | M(e,f) -> l ^ (fmla_as_string e) ^ ") M (" ^ (fmla_as_string f) ^ r
        | Not(e) -> "¬(" ^ (fmla_as_string e) ^ r
        | X(e) -> "X(" ^ (fmla_as_string e) ^ r
        | G(e) -> "G(" ^ (fmla_as_string e) ^ r
        | F(e) -> "F(" ^ (fmla_as_string e) ^ r


(* Reduce formula to only atomic operators/connectives *)
let rec to_atomics x =
    match x with
        | True -> True
        | False -> False
        | Var x -> Var x
        | Not f -> Not (to_atomics f)
        | And(e,f) -> And (to_atomics e, to_atomics f)
        | Or(e,f) -> Or(to_atomics e, to_atomics f)
        | Impl(e,f) -> Or((Not (to_atomics e)), (to_atomics f))
        | Iff(e,f) -> let e' = to_atomics e and f' = to_atomics f in 
            And((Or((Not e'),f')) ,(Or((Not f'), e')))
        | U(e,f) -> U(to_atomics e, to_atomics f)
        | W(e,f) -> let e' = to_atomics e in Or(U(e',to_atomics f), G(e'))
        | R(e,f) -> let f' = to_atomics f in W(f',And(to_atomics e,to_atomics f'))
        | M(e,f) -> Not(W(Not(to_atomics e),Not(to_atomics f)))
        | G(e) -> Not(U(True, (Not (to_atomics e))))
        | X(e) -> X(to_atomics e)
        | F(e) -> U(True, to_atomics e) 

