 exception No_next_frame

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

(* Reduce formula to only atomic operators/connectives. *)
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

(* Evaluate formula x over path w *)
let rec eval_fmla x w =
match x with
    | True -> true
    | False -> false
    | Var v -> List.assoc v (List.hd w)
    | Not(e) -> not (eval_fmla e w)
    | And(e, f) -> (eval_fmla e w) && (eval_fmla f w)
    | Or(e, f) -> (eval_fmla e w) || (eval_fmla f w)
    | U(e, f) -> (let a = eval_fmla e w in
                  let b = eval_fmla f w in
                  match a,b with
                      | false, false -> false
                      | true, false -> (match w with 
                                            | h::t -> eval_fmla f t
                                            | [] -> false )
                      | _ , true -> true )
    | W(e,f) -> eval_fmla (Or(U(e,f), G(e))) w (* e W f ≡ (e U f) ∨ G(e) *)
    | R(e,f) -> eval_fmla (W(f,And(e,f))) w (* e R f ≡ f W (f ∧ e) *)
    | M(e,f) -> eval_fmla (Not(W(Not(e),Not(f)))) w (* e M f ≡ ¬(¬e W ¬f) *)
    | Impl(e,f) -> eval_fmla (Or((Not e), f)) w
    | Iff(e,f) -> eval_fmla (And((Or((Not e),f)) ,(Or((Not f), e)))) w
    | X(e) -> (match w with 
                    | h::t -> eval_fmla e t
                    | [] -> raise No_next_frame )
    | G(e) -> eval_fmla (Not(F(Not e))) w (* Gφ := ¬◊¬φ *)
    | F(e) -> eval_fmla (U(True, e)) w (* ◊φ := true U φ *) 
