open Algo

let epsilon = 0.001

let test_poids =
  let g = Graph.(empty |> add_vertex 1 false (10.,10.) |> add_vertex 2 false (10.,300.) |> add_edge 1 2 ) in
  ((Float.abs (g.poids -. 290.)) <= epsilon)

let test_equal =
  let g = Graph.(empty |> add_vertex 1 false (10.,10.) |> add_vertex 2 false (10.,300.) |> add_edge 2 1) in
  Graph.equal (Graph.remove_edge 2 1 g) (Graph.remove_edge 1 2 g)
  && 
  Graph.equal Graph.empty Graph.empty
  

let test_remove_edge =
  let g = Graph.(empty |> add_vertex 1 true (10.,10.) |> add_vertex 2 true (100.,100.)) in
  let g2 = Graph.(g |> add_edge 1 2) in
  Graph.equal g (Graph.remove_edge 2 1 g2)

let test_remove_vertex =
  let g = Graph.(empty |> add_vertex 1 true (10.,10.)) in
  let g2 = Graph.(g |> add_vertex 2 true (100.,100.) |> add_edge 1 2) in
  Graph.equal g (Graph.remove_vertex 2 g2)



let _ =
  let tests = [
    ("poids", test_poids);
    ("equal", test_equal);
    ("remove_edge", test_remove_edge);
    ("remove_vertex", test_remove_vertex);
  ] in

  let res = 
    List.fold_left (fun acc (name, res) ->
      let result = if res then "OK" else "ERROR" in
      Printf.printf "Test %s %s\n" name result;
      acc && res 
    ) true tests
  in
  exit (if res then 0 else 1)


