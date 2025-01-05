module IntSet : Set.S with type elt = int

val draw_edge : (int * int) * (int * int) -> unit

val draw_pb : (int * int) list -> unit

val draw_sol : ((int * int) * (int * int)) list -> unit

val draw : int * int -> (int * int) list -> ((int * int) * (int * int)) list -> unit

val draw_steiner : int * int -> (float * float) list -> ((float * float) * (float * float)) list -> unit
