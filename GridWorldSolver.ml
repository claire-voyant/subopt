(* Solves GridWorld instances *)

(*let initDps initial_state gridworld = 
  let h_val = GridWorld.heuristic gridworld initial_state in
  let g = 0.0 in
  let f = g +. h_val in
  let init_node = BucketOpenList.{
      f = f;
      g = g;
      state = initial_state;
      plan = [];
      index = (-1);
    } in
  let garb_b = BucketOpenList.garb_bucket init_node in
  let fmin = 99999.9 in
  let open_list = BucketOpenList.create BucketOpenList.sort_b BucketOpenList.node_notify 10000000 fmin garb_b 1.0 in
  BucketOpenList.insert open_list init_node;
  open_list
*)
(*let bucketOpenList initial_state h w bound =
  let h_val = GridWorld.heuristic initial_state h w in
  let g = 0.0 in
  let f = g +. h_val in
  let action = -1 in
  let parent = None in
  let fmin = 99999.9 in
  let garb_b = BucketOpenList.garb_bucket in
  let bop = BucketOpenList.create BucketOpenList.sort_b BucketOpenList.node_notify 5 fmin garb_b 1.0 in
  let init_node = {
    f = f;
    g = g;
    state = initial_state;
    plan = [];
    index = (-1);
  } in 
  BucketOpenList.bound := bound;
  BucketOpenList.insert bop init_node;
  bop (* imagine booping the snoot *) *)

(* need to add successors to heap *)
let run_wa_star domain init_state =
  (*print_endline (Printf.sprintf "GridWorld.WeightedAStar\n");*)
  let is_goal = (fun state -> GridWorld.is_goal domain state) in
  let successors = (fun state -> GridWorld.successors domain state) in
  let heuristic = (fun state -> GridWorld.heuristic domain state) in
  WeightedAStar.run init_state is_goal successors heuristic

let run_dps domain init_state bound =
  (*print_endline (Printf.sprintf "GridWorld.DynPSearch\n");*)
  let is_goal = (fun state -> GridWorld.is_goal domain state) in
  let successors = (fun state -> GridWorld.successors domain state) in
  let heuristic = (fun state -> GridWorld.heuristic domain state) in
  DynPSearch.run init_state is_goal successors heuristic bound


(*let run_dps grid h w openlist hashtbl =
  print_endline (Printf.sprintf "GridWorld.DynamicPotentialSearch\n");
  DynPSearch.run grid h w openlist hashtbl GridWorld.is_goal GridWorld.successors GridWorld.heuristic*)


