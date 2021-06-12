(** Typed ordered fuzzy numbers (OFNs) and associated operations, following 
{{: https://arxiv.org/abs/2010.07764}"Rings of Typed Ordered Fuzzy
Numbers."}*)

(** @author Matthew Kukla *)

(** Incompatible OFN type families. *)
exception OFN_type_mismatch

(** An OFN is improper when it has no membership function. *)
exception Improper_OFN

(** Supported families are trapezoidal, Gaussian, and exponential. *)
type family = Trapezoidal | Gaussian | Exponential

(** OFN with type [family]  and essential tuple (a{_u}, b{_u}, a{_d}, b{_d}).
Components of the tuple are represented as individual fields (as opposed to
an element of [float * float * float * float]) for ease-of-access.*)
type tofn = {
  ofn_type : family;
  au : float;
  bu : float;
  ad : float;
  bd : float;
}

(** Determine if two given OFNs are of the same type. *)
val sametype : tofn -> tofn -> bool

(** Application of a binary operator to the essential tuples of two OFNs. *)
val tuplemap : (float -> float -> float) -> tofn -> tofn -> tofn

(** OFN addition. *)
val ( |+| ) : tofn -> tofn -> tofn

(** OFN Subtraction. *)
val ( |-| ) : tofn -> tofn -> tofn

(** OFN multiplication. *)
val ( |*| ) : tofn -> tofn -> tofn

(** OFN division *)
val ( |/| ) : tofn -> tofn -> tofn

(** Check if an OFN is proper. *)
val is_proper : tofn -> bool

(** Determine if an OFN is increasing. *)
val is_increasing : tofn -> bool

(** Determine if an OFN is decreasing. *)
val is_decreasing : tofn -> bool

(** The membership function associated to an OFN.  Raises [Improper_OFN] if the OFN 
is improper. *)
val membership : tofn -> float -> float
