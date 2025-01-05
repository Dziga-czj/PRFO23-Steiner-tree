include Graph

(* on enlève un élément aléatoire de la liste donnée, et on renvoie un tuple contenant l'élément trouvé et la nouvelle liste ainsi obtenue *)
let pick_one_out l =
	let n = List.length l in
	if n = 1 then (List.hd l,[]) else
	let chosen = Random.int (n-1) in
  let (res, _, l) = List.fold_left (fun (res, i, acc) x ->
    if i = chosen then (x, (i+1), acc)
    else (res, (i+1), x::acc)
    ) (List.hd l, 0, []) l in
  (res,l)

(* même chose qu'avec l'algo au-dessus, mais on enlève deux éléments *)
let pick_two set = (* à optimiser peut-être *)
	let l = NodeSet.elements set in
	let (v1, l') = pick_one_out l in
	let (v2, _) = pick_one_out l' in
	(v1, v2)

(* on renvoie un élément aléatoire de la liste donnée *)
let pick_random l =
  let n = List.length l in
	if n = 1 then (List.hd l) else
  List.nth l (Random.int (n-1))

(* renvoie le graphe avec seulement les sommets de base du probleme *)
let get_problem_graph g =
  let l = get_problem g in
  let g',_ = List.fold_left (fun (g,i) x ->
      (add_vertex i false x g, i+1)
    ) (empty,0) l in
  g'

(* on crée un arbre candidat aléatoire. Si le graphe donné n'est pas un graphe ne contenant que les noeuds du problème, on récupère le graphe de problème *)
let arbre_candidat g =
  let g = if (g.pts_r <> NodeSet.empty) || (g.poids <> 0.) then get_problem_graph g else g in

  let l = NodeMap.bindings g.nodes in
  let keys,nodes = List.split l in
  let (v1, l) = pick_one_out keys in
	let (v2, l) = pick_one_out l in
  let g = add_edge v1 v2 g in
  
  let rec aux lies non_lies g =
    if non_lies = [] then g
    else begin
      let (nouv, l) = pick_one_out non_lies in
      let other = pick_random lies in
      aux (nouv::lies) l (add_edge nouv other g)
    end
  in
  aux (v1::v2::[]) l g 

(* trouve un triangle aléatoire de points, c'est à dire un point aléatoire ayant au moins deux voisins, que l'on choisit aléatoirement *)
let get_random_triangle g =
	let n = NodeMap.cardinal g.nodes in
	let l = NodeMap.bindings g.nodes in
		let rec pick i last =
			let (k,ni) = List.nth l i in
			if NodeSet.cardinal ni.succs < 2 then if i = (last-1) then failwith "pas de triangle"
			else
				pick ((i + 1) mod n) last (* on parcours tout les autres noeuds *)
			else 
				let (v1,v2) = pick_two ni.succs in
				(k, v1, v2)
		in 
		let ri = (Random.int (n-1)) in
		pick ri ri
			
(* calcule l'isobarycentre des trois coordonnées entrées, pondérées par les poids a,b et c *)
let get_isobarycentre (x1,y1) (x2,y2) (x3,y3) (a,b,c) =
	(* ici a, b et c sont les poids des différents points *)
	let div = a +. b +. c in
	let x = (a*.x1 +. b*.x2 +. c*.x3) /. div in
	let y = (a*.y1 +. b*.y2 +. c*.y3) /. div in
	(x,y)

(* trouve la prochaine clé libre pour la map de noeuds *)
let get_new_key g = 
	1 + NodeMap.fold (fun k _ acc -> if k > acc then k else acc) g.nodes 0


(* on perturbe l'arbre en ajoutant un point relais *)
let add_point_relais g =
	(* on calcule l'isobarycentre avec une pondération égale pour les trois points *)
	let (n, v1, v2) = get_random_triangle g in
	let n_node = get_node n g in
	let v1_node = get_node v1 g in
	let v2_node = get_node v2 g in
	let (x,y) = get_isobarycentre n_node.coords v1_node.coords v2_node.coords (1.,1.,1.) in
	let g' = (g |> remove_edge n v1 |> remove_edge n v2 |> remove_edge v1 v2) in
	(* dernier remove au cas où, le remove est sensé ne rien faire si l'arete n'existe pas *)
	let nouveau = (get_new_key g) in
	(g' |> add_vertex nouveau true (x,y) |>
		add_edge nouveau n |>
		add_edge nouveau v1 |>
		add_edge nouveau v2
	)

(* retourne un élément aléatoire du set donné *)
let get_random_elt set =
	let nb = NodeSet.cardinal set -1 in
	let n = if nb = 0 then 0 else Random.int nb in
	let (_,res) = NodeSet.fold (fun x (i,chosen) -> 
		if i = n then (i+1, x)
		else (i+1, chosen)
		) set (0,-1) in
	if res = -1 then failwith "get_random_elt ne devrait pas arriver"
	else res

(* récupère le premier point relais en partant de i dans la liste de clés données *)
let rec pick_not_relais l i g =
	let nth = List.nth l i in
	let n = get_node nth g in
	if not n.is_r then nth else pick_not_relais l ((i+1) mod (List.length l)) g

(* on perturbe l'arbre en trouvant un point relais et en le fusionnant aléatoirement avec un de ses voisins *)
let fusionne_point_relais g =
	let r_chosen = get_random_elt g.pts_r in
	let rel_n = get_node r_chosen g in
	let not_rel_chosen = get_random_elt rel_n.succs in

	let g' = NodeSet.fold (fun x acc ->
		if x <> not_rel_chosen then
			add_edge x not_rel_chosen acc
		else acc
		) rel_n.succs g in
	remove_vertex r_chosen g'


(* on perturbe l'arbre en déplacant un point relais. Pour faire cela, on recalcule l'isobarycentre de base mais en prenant des poids aléatoires. *)
let depl_point_relais g =
	(* on déplace le point relais (qui est un isobarycentre) avec une nouvelle pondération entre tout ses voisins *)
	let r_chosen = get_random_elt g.pts_r in
	let r_node = get_node r_chosen g in
	let (x,y, tot) = NodeSet.fold (fun n (x, y, tot) -> 
		let nd = get_node n g in
		let (x',y') = nd.coords in
		let poids = Random.int 100 in
		(x +. x' *. (float_of_int poids), y +. y' *. (float_of_int poids), tot + poids)
		) r_node.succs (0.,0., 0) in
	let new_coords = (x /. (float_of_int tot), y /. (float_of_int tot)) in
	update_vertex r_chosen true new_coords r_node.succs g
	
(* on perturbe aléatoirement le graphe avec un des trois algos de perturbation. Si l'algo n'a pas de points relais, alors on en rajoute un aléatoirement (premier algo) *)
let perturbate g =
	let i = if g.pts_r = NodeSet.empty then 0 else Random.int 2 in
	match i with 
	| 0 -> add_point_relais g
	| 1 -> fusionne_point_relais g
	| _ -> depl_point_relais g


(* algorithme de hill climbin naif sur n itérations *)
let hill_climbing n g =
  let g = arbre_candidat g in 
  let rec aux_hill_climbing g n =
    if n = 0 then g 
    else
      let g' = perturbate g in 
      let res = if g'.poids < g.poids then g' else g in
      aux_hill_climbing res (n-1)
    in aux_hill_climbing g n

(* algo de hill climbing, mais par back tracking. On parcourt l'arbre des possibilités, et on choisit le meilleur chemin *)   
let hill_climbing_backtracking prof g =
  let g = arbre_candidat g in

  let rec aux_hill_climbing_backtracking g prof =
    if prof = 0 then g
    else
      if g.pts_r = NodeSet.empty then aux_hill_climbing_backtracking (add_point_relais g) (prof-1)
      else
        let g1 = aux_hill_climbing_backtracking (add_point_relais g) (prof-1) in
        let g2 = aux_hill_climbing_backtracking (fusionne_point_relais g) (prof-1) in
        let g3 = aux_hill_climbing_backtracking (depl_point_relais g) (prof-1) in
        if g1.poids <= g2.poids && g1.poids <= g3.poids then g1
        else if g2.poids <= g1.poids && g2.poids <= g3.poids then g2
        else g3
  in aux_hill_climbing_backtracking g prof

(* on effectue l'algo de back tracking mais sur un nombre différent d'arbres candidats différents *)
let h_c_backtracking_different_tries nb_tries prof g =
  let problem = g in
  let res = arbre_candidat g in
  let res = hill_climbing_backtracking prof res in

  let rec aux i old =
    if i >= nb_tries then old else 
      let res = arbre_candidat problem in
      let res = hill_climbing_backtracking prof res in
      if res.poids < old.poids then aux (i+1) res
      else aux (i+1) old
  in aux 2 res
