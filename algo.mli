include module type of Graph

(*
    Returns a random candidate tree with the given problem representing graph

    @requires : a problem representing graph, otherwise it will recalculate it, which is slower
    @ensures : the returned graph is a candidate tree
    @raises : /
*)
val arbre_candidat : graph -> graph

(*
    Recursively performs n random iterations of the hill_climbing algorithm, with a random perturbation at each step.

    @requires : a graph representing a tree, with a properly calculated weight. Otherwise, there could be non specified behaviours.
    @ensures : /
    @raises : /
*)
val hill_climbing : int -> graph -> graph

(*
    Performs a recursive backtracking algorithm, on a random beginning candidate tree. It tests all the different possibilities of a tree of the depth given.
    This still remains random, due to the random nature of the perturbations and the beginning random candidate tree

    @requires : same requirements as the simple hill_climbing algorithm.
    @ensures : /
    @raises : /
*)
val hill_climbing_backtracking : int -> graph -> graph

(*
    Performs the backtracking aglorithm on n different beginning random candidate trees, at the given depth.

    @requires : same requirements as the simple hill_climbing algorithm.
    @ensures : The random beginning candidate trees are not checked to be explicitely different.
    @raises : /
*)
val h_c_backtracking_different_tries : int -> int -> graph -> graph