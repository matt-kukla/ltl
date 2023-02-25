 exception No_next_state

type expr = 
    | True
    | False
    | Var of string
    | And of expr * expr
    | Or of expr * expr
    | Impl of expr * expr
    | Iff of expr * expr
    | U of expr * expr      (* until *)
    | R of expr * expr      (* release *)
    | W of expr * expr      (* weak *)
    | M of expr * expr      (* strong release *)
    | Not of expr 
    | X of expr             (* ◯f (next) *)
    | G of expr             (* □f (always) *)
    | F of expr             (* ◊f  (eventually) *)

let rec eval_fmla f w =
match f with
    | True -> true
    | False -> false
    | Var v -> List.assoc v (List.hd w)
    | Not(e) -> not (eval_fmla e w)
    | And(l, r) -> (eval_fmla l w) && (eval_fmla r w)
    | Or(l, r) -> (eval_fmla l w) || (eval_fmla r w)
    | U(l, r) -> (let a = eval_fmla l w in
                  let b = eval_fmla r w in
                  match a,b with
                      | false, false -> false
                      | true, false -> (match w with 
                                            | _::t -> eval_fmla r t
                                            | [] -> false )
                      | _ , true -> true )
    | W(l,r) -> eval_fmla (Or(U(l,r), G(l))) w (* e W f ≡ (e U f) ∨ G(e) *)
    | R(l,r) -> eval_fmla (W(r,And(l,r))) w (* e R f ≡ f W (f ∧ e) *)
    | M(l,r) -> eval_fmla (Not(W(Not(l),Not(r)))) w (* e M f ≡ ¬(¬e W ¬f) *)
    | Impl(l,r) -> eval_fmla (Or((Not l), r)) w
    | Iff(l,r) -> eval_fmla (And((Or((Not l),r)) ,(Or((Not r), l)))) w
    | X(e) -> (match w with 
        | _::t -> eval_fmla e t
        | [] -> raise No_next_state )
    | G(e) -> eval_fmla (Not(F(Not e))) w (* Gφ := ¬◊¬φ *)
    | F(e) -> eval_fmla (U(True, e)) w (* ◊φ := true U φ *) 

let rec modal_depth f = 
match f with
    | X(e) | F(e) | G(e) -> 1 + (modal_depth e)
    | True | False | Var _ -> 0
    | Not(e) -> modal_depth e
    | And(l,r) | Or(l,r) | U(l,r) 
    | W(l,r) | R(l,r) | M(l,r) | Impl(l,r)
    | Iff(l,r) -> max (modal_depth l) (modal_depth r)

let rec to_nnf f = 
match f with
    | True -> True
    | False -> False
    | Var x -> Var x
    | Not e -> (match e with
        | True -> False
        | False -> True
        | Var x -> Not(Var x)
        | Not e -> Not(e) (* by this point, e is atomic *)
        | And(l,r) -> Or(to_nnf (Not l), to_nnf r)
        | Or(l, r) -> And(to_nnf(Not l), to_nnf (Not r))
        | Impl(l, r) -> And((to_nnf l), to_nnf (Not r))
        | Iff(l,r) -> let l' = to_nnf l and r' = to_nnf r in
            Or(And(l', to_nnf (Not(r'))),And(to_nnf (Not l'), r'))
        | X(l) -> X(to_nnf (Not l))
        | F(l) -> G(to_nnf (Not l))
        | G(l) -> to_nnf (Not(U(True,to_nnf l )))
        | U(l, r) -> R(to_nnf(Not l), to_nnf (Not r)) 
        | R(l,r) -> U(to_nnf (Not l), to_nnf (Not r))
        | W(l,r) -> M(to_nnf (Not l), to_nnf (Not r))
        | M(l, r) -> W(to_nnf (Not l), to_nnf (Not r))) 
    | And(l, r) -> And(to_nnf l, to_nnf r)
    | Or(l, r) -> Or(to_nnf l, to_nnf r)
    | Impl(l, r) -> let l' = to_nnf (Not(l)) and r' = to_nnf r in Or(l', r')
    | Iff(l,r) -> Or(And(to_nnf l, to_nnf (Not r)), And( to_nnf (Not l), to_nnf
    r))
    | U(l, r) -> U(to_nnf l, to_nnf r)
    | R(l, r) -> R(to_nnf l, to_nnf r)
    | W(l,r) -> let l' = to_nnf l and r' = to_nnf r in U(l', (And(l', X r'))) 
    | M(l, r) -> let l' = to_nnf l and r' = to_nnf r in U(l', And(l',r'))
    | X(e) -> X(to_nnf e)
    | G(e) -> to_nnf (Not(U(True,Not (to_nnf e))) )
    | F(e) -> U(True, to_nnf (Not e))

let rec to_atomics f =
    match f with
        | True -> True
        | False -> False
        | Var x -> Var x
        | Not e -> Not (to_atomics e)
        | And(l,r) -> And (to_atomics l, to_atomics r)
        | Or(l,r) -> Or(to_atomics l, to_atomics r)
        | Impl(l,r) -> Or((Not (to_atomics l)), (to_atomics r))
        | Iff(l,r) -> let l' = to_atomics l and r' = to_atomics r in 
            And((Or((Not l'),r')) ,(Or((Not r'), l')))
        | U(l,r) -> U(to_atomics l, to_atomics r)
        | W(l,r) -> let l' = to_atomics l in Or(U(l',to_atomics r), G(l'))
        | R(l,r) -> Not(U(Not l, Not r))
        | M(l,r) -> let l' = to_atomics l and r' = to_atomics r in U(l', And(l',r'))
        | G(e) -> Not(U(True, (Not (to_atomics e))))
        | X(e) -> X(to_atomics e)
        | F(e) -> U(True, to_atomics e) 

let rec fmla_as_string f = 
let pl = "(" and pr = ")" in
    match f with
        | True -> "⊤"
        | False -> "⊥"
        | Var x -> x
        | And(l,r) -> pl ^ (fmla_as_string l) ^ " ∧ " ^ (fmla_as_string r) ^ pr
        | Or(l,r) -> pl ^ (fmla_as_string l) ^ " v " ^ (fmla_as_string r) ^ pr
        | Impl(l,r) -> pl ^ (fmla_as_string l) ^ " → " ^ (fmla_as_string r) ^ pr
        | Iff(l,r) -> pl ^ (fmla_as_string l ) ^ " ↔ " ^ (fmla_as_string r) ^ pr
        | U(l,r) -> pl ^ (fmla_as_string l) ^ ") U (" ^ (fmla_as_string r) ^ pr
        | R(l,r) -> pl ^ (fmla_as_string l) ^ ") R (" ^ (fmla_as_string r) ^ pr
        | W(l,r) -> pl ^ (fmla_as_string l) ^ ") W (" ^ (fmla_as_string r) ^ pr
        | M(l,r) -> pl ^ (fmla_as_string l) ^ ") M (" ^ (fmla_as_string r) ^ pr
        | Not(e) -> "¬(" ^ (fmla_as_string e) ^ pr
        | X(e) -> "X(" ^ (fmla_as_string e) ^ pr
        | G(e) -> "G(" ^ (fmla_as_string e) ^ pr
        | F(e) -> "F(" ^ (fmla_as_string e) ^ pr
