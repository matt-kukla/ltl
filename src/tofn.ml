exception OFN_type_mismatch
exception Improper_OFN

type family = Trapezoidal | Gaussian | Exponential 

type tofn = {
    ofn_type : family; 
    au: float; 
    bu : float;
    ad : float; 
    bd : float; 
}

let sametype x y = x.ofn_type == y.ofn_type

let tuplemap f x y = {  
    ofn_type = x.ofn_type; 
    au = f x.au y.au; 
    bu = f x.bu y.bu;
    ad =  f x.ad y.ad; 
    bd = f x.bd y.bd;
}

(* Arithmetic operations *)
let (|+|) x y = 
    if sametype x y then tuplemap (+.) x y else raise OFN_type_mismatch

let (|-|) x y = 
     if sametype x y then tuplemap (-.) x y else raise OFN_type_mismatch

let (|*|) x y = 
     if sametype x y then tuplemap ( *. ) x y else raise OFN_type_mismatch

let (|/|) x y = 
    if y.au == 0. || y.bu == 0. || y.ad == 0. || y.bd == 0. then raise Division_by_zero 
    else if sametype x y then tuplemap (/.) x y else raise OFN_type_mismatch

let is_proper x = (x.au <= x.bu  && x.bu <= x.ad && x.ad <= x.bd) ||
                  (x.au >= x.bu  && x.bu >= x.ad && x.ad >= x.bd) ||
                  (x.au = 0. && x.ad = 0. && x.bu <> 0. && x.bd <> 0.)

let is_increasing x = (is_proper x) && (x.au > 0.) && (x.ad < 0.)

let is_decreasing x = (is_proper x) && (x.au < 0.) && (x.ad > 0.)

let membership x = 
    if not (is_proper x) then raise Improper_OFN else
    let mem f fi y =
        if is_increasing x then
            if y >= (x.au *. (f 0.) +. x.bu) && y < (x.au *. (f 1.) +. x.bu) 
                then fi ((y -. x.bu) /. x.au)
            else if y > (x.ad *. (f 1.) +. x.bd) &&  y <= (x.au *. (f 0.) +. x.bu)
                then fi ((y -. x.bd) /. x.ad)
            else 1. 

        else if (is_decreasing x) then
            if y >= (x.au *. (f 0.) +. x.bu) && y < (x.au *. (f 1.) +. x.bu)
                then fi ((y -. x.bd) /. x.ad)
            else if y > (x.ad *. (f 1.) +. x.bd) &&  y <= (x.au *. (f 0.) +. x.bu)
                then fi ((y -. x.bu) /. x.au)
            else 1.
        
        else raise Improper_OFN

    in 
        match x.ofn_type with
            | Trapezoidal -> (mem (fun z ->  z) (fun z -> z))  
            | Gaussian -> (mem (fun z -> -2. *. log z) (fun z -> -0.5 *. (z**2.))) 
            | Exponential -> (mem (fun z -> exp z) (fun z -> log z))

