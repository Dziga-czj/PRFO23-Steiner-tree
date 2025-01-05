module NodeSet = Set.Make(Int)
module NodeMap = Map.Make(Int)

type node = { is_r : bool; coords: (float*float); succs : NodeSet.t}
type graph = { poids: float; nodes : node NodeMap.t; pts_r: NodeSet.t }

let empty = { poids = 0.; nodes = NodeMap.empty; pts_r = NodeSet.empty }

let is_empty g =
	(g.poids = 0.) && (NodeMap.is_empty g.nodes) && (NodeSet.is_empty g.pts_r)

let get_node n g = 
	NodeMap.find n g.nodes

let get_poids g = g.poids

let get_pts_relais g = g.pts_r

let get_node_coords n g =
	(get_node n g).coords

let get_node_opt n g =
	NodeMap.find_opt n g.nodes

let succs n g =
	(get_node n g).succs

let fold f g v0 =
	NodeMap.fold (fun v _ acc -> f v acc) g.nodes v0

let calc_dist (src_coords : float*float) (dst_coords : float*float) =
	let x1,y1 = src_coords in
	let x2,y2 = dst_coords in
	sqrt ((x1-.x2)*.(x1-.x2) +. (y1-.y2)*.(y1-.y2)) 
	
let node_dist src dst =
	calc_dist src.coords dst.coords

let get_edge_poids src dst g =
	node_dist (get_node src g) (get_node dst g)

let add_edge src dst g = (*is double sided*)
	match (get_node_opt src g ) with 
	| None -> failwith "src does not exist in g"
	| _ -> match (get_node_opt dst g) with
			| None -> failwith "dst does not exist in g"
			| _ ->begin 

				(*on met à jour le poids*)
				let poids' = g.poids +. (get_edge_poids src dst g) in

				(*on met à jour les nouveaux points : *)
				let src_n = get_node src g in (*src node*)
				let dst_n = get_node dst g in (*dst node*)

				let src_node = { is_r = src_n.is_r; coords = src_n.coords; succs = NodeSet.add dst src_n.succs} in
				let dst_node = { is_r = dst_n.is_r; coords = dst_n.coords; succs = NodeSet.add src dst_n.succs} in
				{ poids = poids'; nodes = NodeMap.add src src_node (NodeMap.add dst dst_node g.nodes); pts_r = g.pts_r }
			end

let add_vertex n is_relais coords g = (* ne fais rien si n a déjà un vertex lié *)
	match (NodeMap.find_opt n g.nodes) with
	| Some(_) -> g
	| None ->
		let map' = NodeMap.add n {is_r = is_relais; coords=coords; succs = NodeSet.empty} g.nodes in
		let pts_r' = if is_relais then NodeSet.add n g.pts_r else g.pts_r in 
		{poids = g.poids; nodes = map'; pts_r = pts_r'}

let remove_edge src dst g =
	let src_n = get_node src g in
	let dst_n = get_node dst g in
	if NodeSet.mem dst src_n.succs then begin (* on vérifie que l'arete existe vraiment, sinon on ne fait rien *)
		let nodes = NodeMap.add src { is_r = src_n.is_r; coords = src_n.coords; succs = NodeSet.remove dst src_n.succs} g.nodes in
		let nodes2 = NodeMap.add dst { is_r = dst_n.is_r; coords = dst_n.coords; succs = NodeSet.remove src dst_n.succs} nodes in
		{ poids = g.poids -. (get_edge_poids src dst g); nodes = nodes2; pts_r = g.pts_r }
	end
	else g

let remove_vertex n g =
	let nd = get_node n g in
	if not nd.is_r then failwith "remove_vertex on not relais"
	else begin
			let g' = NodeSet.fold (fun x acc ->
				remove_edge n x acc
			) nd.succs g in
			{ poids = g'.poids; nodes = NodeMap.remove n g'.nodes; pts_r = NodeSet.remove n g'.pts_r }
		end

let update_vertex n is_relais coords succs g =
	let g' = remove_vertex n g in
	let g2 = add_vertex n is_relais coords g' in
	NodeSet.fold (fun x acc ->
		add_edge n x acc
		) succs g2

let mem_vertex n g =
	NodeMap.mem n g.nodes

let mem_edge src dst g = 
	if mem_vertex src g then
		let succs_src = succs src g in
		NodeSet.mem dst succs_src
	else
		false

let print g =
	NodeMap.iter (fun key n ->
		Printf.printf "Node %d -> " key;
		NodeSet.iter (fun x -> 
			Printf.printf "%d " x) n.succs;
		Printf.printf "\n"
	) g.nodes;
	Printf.printf "poids : %4f\n" g.poids;
	flush stdout

let print_poids g =
	Printf.printf "poids : %f\n" g.poids;
	flush stdout

let get_vertexes_coords g =
	NodeMap.fold (fun _ x acc -> 
		x.coords::acc
	) g.nodes []

let get_edges_coords g =
	NodeMap.fold (fun k n acc -> 		
		(* on fait la liste des edges -> doublons pour l'instant (a->b et b->a) *)
		let l = NodeSet.fold (fun node acc2 ->
			let curr = get_node node g in
			(curr.coords, n.coords)::acc2
			) n.succs [] in

			List.append acc l
		) g.nodes []

let equal_coords (x1,y1) (x2,y2) =
	(x1 = x2) && (y1 = y2)

let equal_nodes n1 n2 = 
	(n1.is_r = n2.is_r) && (equal_coords n1.coords n2.coords) && (NodeSet.equal n1.succs n2.succs)

let equal g1 g2 = 
	(g1.poids = g2.poids) && (NodeSet.equal g1.pts_r g2.pts_r) && ( NodeMap.equal equal_nodes g1.nodes g2.nodes )


let get_problem g =
	NodeMap.fold (fun _ x acc -> 
		if not x.is_r then
			x.coords::acc
		else acc
	) g.nodes []
