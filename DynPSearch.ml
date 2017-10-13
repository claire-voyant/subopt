(* Dynamic potential search implementation *)

(* 

outline of pseudocode from paper

paper described the algorithm as a focal-search but
uses the same ChooseNode rule as potential search

the proof in the paper makes the argument that the
algorithm does not need to implement a focal list
similar to weighted A* 

the open list is maximal ordered with the potential function:

ud(n) = (B x f_min - g(n)) / h(n)

where B is the suboptimality bound (we usually call it w)

f_min is the smallest f value on open f(n) = g(n) + h(n)

g(n) and h(n) are the usual cost-so-far and lower bound on
the cost-to-go towards a goal

DynPSearch (s_init):
(
  open <- {s_init}; 
  while open isNotEmpty do
    best <- ChooseNode(open)
    remove best from open
    if best is a goal then return best;
    if f_min has increased then reorder open;
    for each n successor of best do
      add n to open
      if f(n) <= B x f_min then add n to open     
    done
  done
)

*)

type ('state, 'action) node = {
  mutable f:float; (* actually f *) 
  mutable g:float;
  state:'state;
  mutable plan:'action list; (* reverse ordered *)
  mutable index:int;
}

let update_fg n f g =
  n.f <- f;
  n.g <- g

let print_node n p sa =
  let f = n.f in
  let g = n.g in
  (*let s = p n.state in*)
  (*let i = n.index in*)
  print_string (Printf.sprintf "f= %f\n\ng = %f\n\nplan: " f g);
  List.iter (fun act -> print_string (sa act)) (List.rev n.plan);
  print_endline "\n\nSuccess!\n"

let p_node n =
  let f = n.f in
  let g = n.g in
  let i = n.index in
  (Printf.sprintf "f= %f | g = %f | i = %d\n" f g i)

let node_notify n i = ()

let index_f e n = e.index <- n

let get_i n = n.index

let get_f n = n.f

let get_g n = n.g

let addToExisting openlist closedlist g f state act plan gen = 
  let succ_node = {
    f = f;
    g = g;
    state = state; 
    plan = act::plan;
    index = (-1);
  } in
  ExperimentRecord.inc_gen gen ();
  BucketOpenList.insert openlist succ_node;
  Hashtbl.add closedlist state succ_node

let evaluateSuccessor heuristic openlist closedlist succ act act_cost curnode nodeStats =
  if (BucketOpenList.checkFMin openlist) then  
    BucketOpenList.fixOpenList openlist;
  let succ_node = {
    f = curnode.g +. act_cost +. heuristic succ;
    g = curnode.g +. act_cost;
    state = succ;
    plan = act::(curnode.plan);
    index = (-1);
  } in
  try
    let node_lookup = Hashtbl.find closedlist succ in
    if node_lookup.g > succ_node.g then
      (
        (* found a better path update the existing node *)
        BucketOpenList.replace openlist node_lookup succ_node p_node;
      )
  with Not_found ->
    (* found a new node generate it and add it to the existing open *)
    (addToExisting openlist closedlist succ_node.g succ_node.f succ act curnode.plan nodeStats)

let expandCurrentNode openlist nodeStats goal_check heuristic closedlist successors = 
  let rec expand () = 
    (*BucketOpenList.print openlist;*)
    (* updates to the current node in the search*)
    let curnode = BucketOpenList.chooseNode openlist in
    ExperimentRecord.inc_exp nodeStats ();
    (* performs ChooseNode until at a goal *)
    (* dps pseudocode checks goal within the loop *)
    if ( ( (BucketOpenList.isNotEmpty openlist)) && (goal_check curnode.state) ) then
      curnode, (ExperimentRecord.get_exp nodeStats ())
    else
      (
        (* did not find a goal so proceed with search, expand successors *)
        List.iter ( fun (succ, act, act_cost) ->
            (* checks if fmin has changed and fixes open *)
            (evaluateSuccessor heuristic openlist closedlist succ act act_cost curnode nodeStats)
          ) (successors curnode.state);
        expand ()
      ) in
  (*print_endline (Printf.sprintf "Beginning to plan..wish me luck...\n");*)
  expand()

let createStructures bound init_state init_node =
  let openlist = BucketOpenList.create init_node bound index_f get_f get_g get_i p_node in
  let closedlist = Hashtbl.create 1000000 in
  BucketOpenList.insert openlist init_node;
  Hashtbl.add closedlist init_state init_node;
  (openlist, closedlist, (ExperimentRecord.create ()))

let run init_state goal_check successors heuristic bound =
  let h_val = heuristic init_state in
  let init_node = {
    f = 0.0 +. h_val;
    g = 0.0;
    state = init_state;
    plan = [];
    index = (-1);
  } in
  let (openlist, closedlist, nodeStats) = createStructures bound init_state init_node in
  expandCurrentNode openlist nodeStats goal_check heuristic closedlist successors

