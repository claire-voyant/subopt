(* Solves SlidingTile instances *) 

(*let initWAStar initial_state tiles = 
  let h_val = SlidingTile.heuristic tiles initial_state in
  let g = 0.0 in
  let f = g +. h_val in
  let init_node = WeightedAStar.{
      f = f;
      g = g;
      state = initial_state;
      plan = [];
    } in
  let open_list = Dpq.create WeightedAStar.sort_f WeightedAStar.node_notify 10000000 init_node in
  Dpq.insert open_list init_node;
  open_list
*)

let run_wa_star domain init_state = 
  (*print_endline (Printf.sprintf "SlidingTile.WeightedAStar\n");*)
  let is_goal = (fun state -> SlidingTile.is_goal domain state) in
  let successors = (fun state -> SlidingTile.successors domain state) in
  let heuristic = (fun state -> SlidingTile.heuristic domain state) in
  WeightedAStar.run init_state is_goal successors heuristic

let run_dps domain init_state bound = 
  (*print_endline (Printf.sprintf "SlidingTile.DynPSearch\n");*)
  let is_goal = (fun state -> SlidingTile.is_goal domain state) in
  let successors = (fun state -> SlidingTile.successors domain state) in
  let heuristic = (fun state -> SlidingTile.heuristic_dps domain state) in
  DynPSearch.run init_state is_goal successors heuristic bound


