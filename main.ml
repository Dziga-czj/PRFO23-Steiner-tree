open Display
open Algo
open Io_handler

let size = (800,800)

let default_options = ((800,800), 3, 30, 9, false , "", false)

let steiner () =
  let l = Io_handler.read () in
  let g,_ = List.fold_left (fun (g,i) x ->
      (add_vertex i false x g, i+1)
    ) (empty,0) l in

  let g = h_c_backtracking_different_tries 9 30 g in
  
  Display.draw_steiner size (Algo.get_problem g) (Algo.get_edges_coords g);
  print_poids g;
  Graphics.close_graph ();
  get_edges_coords g


let get_next_int l =
  match l with
  | [] -> failwith "check usage"
  | t::q -> (int_of_string_opt t, q)

let handle_options () =
  let l = Array.to_list Sys.argv in

  let rec aux curr acc =
    let ((x,y), a, n, prof, w, file, o) = acc in
    match curr with
    | [] -> acc
    | t::q -> match t with
              | "-s" -> begin 
                        let (x, q2) = get_next_int q in
                        let (y, q3) = get_next_int q2 in
                        match x with
                        | None -> failwith "usage : -s <x> <y>"
                        | Some(x') -> begin match y with
                                    | None -> failwith "usage : -s <x> <y>"
                                    | Some(y') -> aux q3 ((x',y'), a, n, prof, w, file, o) end
              end
              | "-a" -> begin 
                        let (v, q2) = get_next_int q in
                        match v with
                        | Some(value) -> if a > 3 then failwith "usage : -a [1-3]" else aux q2 ((x,y), value, n, prof, w, file, o)
                        | None -> aux q acc
              end
              | "-n" -> begin
                        let (v, q2) = get_next_int q in
                        match v with
                        | Some(value) -> aux q2 ((x,y), a, value, prof, w, file, o)
                        | None -> aux q acc
              end
              | "-d" -> begin
                        let (v, q2) = get_next_int q in
                        match v with
                        | Some(value) -> aux q2 ((x,y), a, n, value, w, file, o)
                        | None -> aux q acc
              end
              | "-w" -> aux q ((x,y), a, n, prof, true, file, o)
              | "-f" ->  begin
                        match q with
                        | [] -> failwith "usage : -f <file>"
                        | t2::q2 -> let (v, q2) = get_next_int q in
                                    match v with
                                    | Some(_) -> failwith "usage : -f <file>"
                                    | None -> aux q2 ((x,y), a, n, prof, w, t2, o)
              end
              | "-o" -> aux q ((x,y), a, n, prof, w, file, true)
              | _ -> failwith "check usage"
              


  in aux (List.tl l) default_options

let _ =
  (* on ouvre et ferme pour mettre à jour la fenêtre *)
  Graphics.open_graph "";
  Graphics.close_graph ();

  Random.init (int_of_float (Sys.time () *. 10000.));
  
  let ((x,y), a, n, prof, w, file, o) = handle_options () in

  if (size, a, n, prof, w, file, o) = default_options then steiner ()
  else

    let l = 
      if "" <> file then 
        read_coords_file file
      else
        read () 
    in

    let g,_ = List.fold_left (fun (g,i) x ->
        (add_vertex i false x g, i+1)
      ) (empty,0) l in

    let g =
      match a with
      | 1 -> hill_climbing n g 
      | 2 -> hill_climbing_backtracking prof g
      | 3 -> h_c_backtracking_different_tries n prof g
      | _ -> g
    in

    if o then begin
      match a with
      | 1 -> begin 
      Printf.printf "using the hill_climbing algorithm\n";
      Printf.printf "parameters : n=%d\n" n;
      end
      | 2 -> begin
      Printf.printf "using the hill_climbing_backtracking algorithm\n";
      Printf.printf "parameters : prof=%d\n" prof;
      end
      | 3 -> 
      begin
      Printf.printf "using the backtracking_different_tries algorithm\n";
      Printf.printf "parameters : n=%d prof=%d\n" n prof;
      end
      | _ -> ()
    end;
    Display.draw_steiner size (Algo.get_problem g) (Algo.get_edges_coords g);
    

    if w then print_poids g;


    Graphics.close_graph ();

    get_edges_coords g



 
  