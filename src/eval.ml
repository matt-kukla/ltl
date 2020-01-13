open Formulae

exception NoNextFrame

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
                    | [] -> raise NoNextFrame )
    | G(e) -> eval_fmla (Not(F(Not e))) w (* Gφ := ¬◊¬φ *)
    | F(e) -> eval_fmla (U(True, e)) w (* ◊φ := true U φ *) 
