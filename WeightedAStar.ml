(* WeightedA* implementation *)

(*
 * basic vanilla A* algorithm becomes a weighted A*
 * when the heuristic provided is weighted by some w
 *
 * openlist - a priority queue for ordering the nodes by f
 * closedlist - closed list implemented as a hash table
 * goal_check - is_goal function for checking if a state is a goal
 * successors - successor function of the domain (expand function)
 * heuristic - heuristic function of the domain (lower bound on cost to go)
 *)

type ('state, 'action) node = {
  mutable f:float; (* actually f prime *) 
  mutable g:float;
  state:'state;
  mutable plan:'action list; (* reverse ordered *) 
}

let fail_node s = {
  f = 0.0;
  g = 0.0;
  state = s;
  plan = [];
}

let sort_f a_node b_node =
  if a_node.f < b_node.f then
    true
  else if a_node.g < b_node.g then  
    false
  else 
    false

let node_notify n i = ()

let print_node n p sa =
  let f = n.f in
  let g = n.g in
  (*let s = p n.state in*)
  print_string (Printf.sprintf "f = %f\n\ng = %f\n\nplan: " f g);
  List.iter (fun act -> print_string (sa act)) (List.rev n.plan);
  print_endline "\n\nSuccess!\n"

let print_nodee ele =
  let f = ele.f in
  let g = ele.g in
  let h = f -. g in
  print_endline (Printf.sprintf "f=%f\ng=%f\nh=%f\n" f g h)

let addToExisting openlist closedlist h g f state act plan gen = 
  let succ_node = {
    f = f;
    g = g;
    state = state;
    plan = act::plan;
  } in
  ExperimentRecord.inc_gen gen (); 
  Dpq.insert openlist succ_node;
  Hashtbl.add closedlist state succ_node 

let run init_state goal_check successors heuristic =
  let nodeStats = ExperimentRecord.create () in
  let h_val = heuristic init_state in
  let g = 0.0 in
  let f = g +. h_val in
  let init_node = {
    f = f;
    g = g;
    state = init_state;
    plan = [];
  } in
  let openlist = Dpq.create sort_f node_notify 10000 init_node in
  let closedlist = Hashtbl.create 10000000 in
  Dpq.insert openlist init_node;
  Hashtbl.add closedlist (init_state) init_node;
  let rec expand () =
    let curnode = Dpq.extract_first openlist in 
    (*print_endline (Printf.sprintf "Nodes expanded: %d ; Nodes generated: %d ; \n" !nodes_expanded !nodes_generated);*)
    ExperimentRecord.inc_exp nodeStats ();
    (* performs A* expansions until a goal is found *)
    if ( goal_check curnode.state ) then
        curnode, (ExperimentRecord.get_exp nodeStats ())
    else
      (
        (* expand successors *)
        (*Dpq.iter ( fun ele -> print_endline "openlist:\n"; print_nodee ele) openlist;*)
        List.iter (fun (succ, act, act_cost) ->  
            (* generate node for successor *)
            let succ_h = heuristic succ in 
            let succ_g = curnode.g +. act_cost in
            let succ_f = succ_g +. succ_h in 
            try
              let node_lookup = Hashtbl.find closedlist succ in
              if node_lookup.g > succ_g then
                (
                  (* found a better path update the existing node *)
                  node_lookup.f <- succ_f;
                  node_lookup.g <- succ_g;
                  node_lookup.plan <- act::(curnode.plan);
                  (* Hashtbl.replace closedlist succ succ_node;*)
                  Dpq.insert openlist node_lookup;
                )
            with Not_found -> 
              (* found a new node generate it and add it to existing open *)
              (addToExisting openlist closedlist succ_h succ_g succ_f succ act curnode.plan nodeStats)
          ) (successors curnode.state);
        expand (); 
      ) in
  (*print_endline (Printf.sprintf "Beginning to plan...wish me luck...\n");*)
  expand ();


  (*print_endline (Printf.sprintf "OpenList size: %d..." (Dpq.count openlist));*)
  (*Dpq.iter (fun n -> Data.print_node n) openlist; *)

  (*print_endline (Printf.sprintf "---\n");
    print_endline (Printf.sprintf "ClosedList size: %d..." (Hashtbl.length closedlist));*)
  (*Hashtbl.iter (fun k n -> Data.print_node n) closedlist; *)

  (*print_endline (Printf.sprintf "---\n");
    print_endline (Printf.sprintf "Successors:\n---");*)
  (*print_endline (Printf.sprintf "---\n");*)

(*
 * let succ_node = {
              f = succ_f;
              g = succ_g;
              state = succ;
              plan = act::curnode.plan;
              index = -1;
            } in 
            nodes_generated := !nodes_generated + 1;
            (* if we haven't seen this state (node) before expand *)
            if ( not ( Hashtbl.mem closedlist succ_node.state) ) then
              (
                (*print_endline (Printf.sprintf "unseen node generated...");*)
                (* add it to the open list *)
                Dpq.insert openlist succ_node;
                (* put it into the table of nodes we've seen *) 
                Hashtbl.add closedlist succ_node.state succ_node
              )
            else
              (*print_endline (Printf.sprintf "seen node generated...");*)
              (* update node we've seen if it has better cost *)
              let existingSuccessorNode = Hashtbl.find closedlist succ_node.state in 
              if existingSuccessorNode.g > succ_node.g then
                (
                  (*print_endline (Printf.sprintf "better path found...");*)
                  Hashtbl.replace closedlist succ_node.state succ_node;
                  Dpq.insert openlist succ_node;
                )
          )
          (successors curnode.state);

*)
