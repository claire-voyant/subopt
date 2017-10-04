(*
 * basic type to keep track of experiment data
 *)

type node_stats  = {
  mutable nodes_generated : int;
  mutable nodes_expanded : int;
}

let create () = {
  nodes_generated = 0;
  nodes_expanded = 0;
}

let inc_gen t () = 
  t.nodes_generated <- t.nodes_generated + 1

let inc_exp t () =
  t.nodes_expanded <- t.nodes_expanded + 1

let get_exp t () = 
  t.nodes_expanded

let get_gen t () = 
  t.nodes_generated



