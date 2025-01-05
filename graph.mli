module NodeSet : Set.S with type elt = int
module NodeMap : Map.S with type key = int

type node = { is_r : bool; coords: (float*float); succs : NodeSet.t}
type graph = { poids: float; nodes : node NodeMap.t; pts_r: NodeSet.t }

val empty : graph

(*
    checks if the given graph is empty
    
    @requires : /
    @ensures : returns true only if the given graph is a fully empty graph 
    @raises : /
*)
val is_empty : graph -> bool

(*
    returns a new graph with a new empty vertex added to it

    @requires : /
    @ensures : if the key to the vertex is already used, the graph remains unchanged
    @raises : /
*)
val add_vertex : int -> bool -> (float*float) -> graph -> graph

(*
    changes the values of an existing relay vertex in g 

    @requires : that the given key has a node assigned to it in g and that the node is a relay vertex
    @ensures : /
    @raises : failwith "remove_vertex on not relais" if the given key does not point to a relay vertex
*)
val update_vertex : int -> bool -> (float*float) -> NodeSet.t -> graph -> graph

(*
    returns a graph with a newly created double sided edge between the two nodes described by the two given keys

    @requires : the two given keys must have nodes assigned to them in the given graph
    @ensures : the created edge goes both ways (from src to dst and vice-versa)
    @raises : raises "src does not exist in g" or "dst does not exist in g" if one of the given keys has no node in the given graph
*)
val add_edge : int -> int -> graph -> graph

(*
    checks if a key is refering to a node in the given graph

    @requires : /
    @ensures : /
    @raises : /
*)
val mem_vertex : int -> graph -> bool

(*
    checks if there is an edge connecting the two nodes, refered by the two given keys

    @requires : /
    @ensures : only that there is a src -> dst edge (hoping for the graph to be double-sided)
    @raises : /
*)
val mem_edge : int -> int -> graph -> bool

(*
    removes the edge if there is one, between the two nodes refered by the two given keys. Otherwise, this does nothing.

    @requires : that the two keys both refer to nodes in the given graph
    @ensures : /
    @raises : raises an error if one of the keys do not refer to an existing node
*)
val remove_edge : int -> int -> graph -> graph

(*
    removes the vertex refered by the given key in g

    @requires : the refered node has to be a relay node, and has to exist in g
    @ensures : /
    @raises : raises "remove_vertex on not relais" if the given node is not a relay point, and throsw an error if the given keys doesn't refer to any node in g
*)
val remove_vertex : int -> graph -> graph

(*
    prints the given graph by listing every node, and it's neighbours. And print it's weight at the end

    @requires : /
    @ensures : flushes stdout at the end of the function
    @raises : /
*)
val print : graph -> unit

(*
    prints the given graph's weight

    @requires : /
    @ensures : /
    @raises : /
*)
val print_poids : graph -> unit

(*
    returns a list of all the vertexes's coordinates in the given graph

    @requires : /
    @ensures : /
    @raises : /
*)
val get_vertexes_coords : graph -> ((float*float) list)

(*
    returns a list of all the vertexe's coordinates that aren't relay points in the given graph

    @requires : /
    @ensures : /
    @raises : /
*)
val get_problem : graph -> ((float*float) list)

(*
    returns a list of all the coordinates making an edge in the given graph. The edges are given in double (a -> b and b -> a)

    @requires : /
    @ensures : /
    @raises : /
*)
val get_edges_coords : graph -> ((float*float)*(float*float)) list

(*
    returns the weight of the given graph

    @requires : /
    @ensures : doesn't calculate the weight, it only returns the registered value
    @raises : /
*)
val get_poids : graph -> float

(*
    returns the node referd to by the given key in the given graph

    @requires : that the graph has a binding of the given key
    @ensures : /
    @raises : an error if the given graph hasn't a binding for the given key
*)
val get_node : int -> graph -> node

(*
    returns the Some(node) if the given graph has a node refered by the given key, returns None otherwise

    @requires : /
    @ensures : /
    @raises : /
*)
val get_node_opt : int -> graph -> node option

(*
    returns the NodeSet containing all the keys refering to the relay points in the given graph

    @requires : /
    @ensures : doesn't compute the returned set, only returns the precomputed value.
    @raises : /
*)
val get_pts_relais : graph -> NodeSet.t

(*
    checks if the two given graphs are the same.

    @requires : /
    @ensures : /
    @raises : /
*)
val equal : graph -> graph -> bool

