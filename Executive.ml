(*Runs the algorithm with *)

exception ExecutiveError of string

type config = {
  domain : string;
  algorithm : string;
  bound : string;
  height : string;
  width : string;
  problem : string list list;
}

type read_four = {
  a : int;
  b : int;
  c : int;
  d : int;
}

let read_four a b c d = {a=a;b=b;c=c;d=d};;

let run_tiles_wa_star (c:config) = 
  let bound = (float_of_string c.bound) in
  let h = (int_of_string c.height) in
  let w = (int_of_string c.width) in
  let tiles = Array.make (h * w) 0 in
  let b = ref 0 in
  for i=1 to 4 do
    let line = List.nth (List.nth c.problem (i-1)) 0 in
    let tup = Scanf.sscanf line "%d %d %d %d" read_four in
    tiles.(0 + ((i-1) * 4)) <- tup.a;
    if tup.a = 0 then b := (0 + ((i-1) * 4));
    tiles.(1 + ((i-1) * 4)) <- tup.b;
    if tup.b = 0 then b := (1 + ((i-1) * 4));
    tiles.(2 + ((i-1) * 4)) <- tup.c;
    if tup.c = 0 then b := (2 + ((i-1) * 4));
    tiles.(3 + ((i-1) * 4)) <- tup.d;
    if tup.d = 0 then b := (3 + ((i-1) * 4));
  done;
  let puzzle = SlidingTile.make_empty_world (h * w) h w bound in
  let init_state = SlidingTile.{board=tiles;blank=(!b)} in
  (*print_endline (SlidingTile.print_tile_puzzle init_state);*)
  if c.algorithm = "wa*" then
    SlidingTileSolver.run_wa_star puzzle init_state
  else 
    raise (ExecutiveError "Executive.run_tiles_wa_star unimplemented sliding_tile_puzzle dps")

let run_gridworld_wa_star (c:config) = 
  let bound = (float_of_string c.bound) in
  let h = (int_of_string c.height) in
  let w = (int_of_string c.width) in
  let grid = Array.make_matrix h w false in 
  (*print_endline (Printf.sprintf "length grid %d x %d" (Array.length grid) (Array.length (grid.(0))));*)
  for j=0 to (w-1) do
    for i=0 to (h-1) do
      if String.get (List.nth (List.nth c.problem i) 0) j == '#' then
        grid.(i).(j) <- true;
    done; 
  done; 
  (*Array.iter (fun i -> Array.iter(fun j -> print_endline ((string_of_bool j) ^ "\n")) i) grid;*)
  let gridworld = GridWorld.make_empty_world h w grid bound in
  if c.algorithm = "wa*" then
    GridWorldSolver.run_wa_star gridworld GridWorld.{x=0;y=0}
  else
    raise (ExecutiveError "Executive.run_gridworld_wa_star unimplemented configuration\n")
(*GridWorldSolver.run_dps     gridworld (GridWorldSolver.initWAStar GridWorld.{x=0;y=0} gridworld ) hashtbl*)

let run_tiles_dps (c:config) = 
  let bound = (float_of_string c.bound) in
  let h = (int_of_string c.height) in
  let w = (int_of_string c.width) in
  let tiles = Array.make (h * w) 0 in
  let b = ref 0 in
  for i=1 to 4 do
    let line = List.nth (List.nth c.problem (i-1)) 0 in
    let tup = Scanf.sscanf line "%d %d %d %d" read_four in
    tiles.(0 + ((i-1) * 4)) <- tup.a;
    if tup.a = 0 then b := (0 + ((i-1) * 4));
    tiles.(1 + ((i-1) * 4)) <- tup.b;
    if tup.b = 0 then b := (1 + ((i-1) * 4));
    tiles.(2 + ((i-1) * 4)) <- tup.c;
    if tup.c = 0 then b := (2 + ((i-1) * 4));
    tiles.(3 + ((i-1) * 4)) <- tup.d;
    if tup.d = 0 then b := (3 + ((i-1) * 4));
  done;
  let puzzle = SlidingTile.make_empty_world (h * w) h w bound in
  let init_state = SlidingTile.{board=tiles;blank=(!b)} in
  (*print_endline (SlidingTile.print_tile_puzzle init_state);*)
  if c.algorithm = "dps" then
    SlidingTileSolver.run_dps puzzle init_state bound
  else 
    raise (ExecutiveError "Executive.run_tiles_dps unimplemented sliding_tile_puzzle dps")


let run_gridworld_dps (c:config) = 
  let bound = (float_of_string c.bound) in
  let h = (int_of_string c.height) in
  let w = (int_of_string c.width) in
  let grid = Array.make_matrix h w false in 
  (*print_endline (Printf.sprintf "length grid %d x %d" (Array.length grid) (Array.length (grid.(0))));*)
  for j=0 to (w-1) do
    for i=0 to (h-1) do
      if String.get (List.nth (List.nth c.problem i) 0) j == '#' then
        grid.(i).(j) <- true;
    done; 
  done; 
  (*Array.iter (fun i -> Array.iter(fun j -> print_endline ((string_of_bool j) ^ "\n")) i) grid;*)
  let gridworld = GridWorld.make_empty_world h w grid bound in
  if c.algorithm = "dps" then
    GridWorldSolver.run_dps gridworld GridWorld.{x=0;y=0} bound
  else
    raise (ExecutiveError "Executive.run_gridworld_dps unimplemented configuration\n")
(*GridWorldSolver.run_dps     gridworld (GridWorldSolver.initWAStar GridWorld.{x=0;y=0} gridworld ) hashtbl*)


let run (c:config) = 
  print_endline ("Domain: " ^ c.domain ^ "\n");
  print_endline ("Algorithm: " ^ c.algorithm ^ "\n");
  print_endline ("Bound: " ^ c.bound ^ "\n");
  print_endline ("Height: " ^ c.height ^ "\n");
  print_endline ("Width: " ^ c.width ^ "\n");
  List.iter(fun x -> List.iter(fun y -> print_string y; print_newline()) x) c.problem;
  print_endline "\n---\n"

let run_wa_gw (conf) = 
  run (conf);
  let t = Sys.time () in
  let (solution, nodes) = run_gridworld_wa_star (conf) in
  print_endline (Printf.sprintf "Execution time: %fs\n" (Sys.time () -. t));
  print_endline (Printf.sprintf "Nodes Expanded: %d\n" nodes);
  WeightedAStar.print_node solution GridWorld.print_state GridWorld.string_of_action

let run_wa_stp (conf) = 
  run (conf);
  let t = Sys.time () in
  let (solution, nodes) = run_tiles_wa_star (conf) in
  print_endline (Printf.sprintf "Execution time: %fs\n" (Sys.time () -. t));
  print_endline (Printf.sprintf "Nodes Expanded: %d\n" nodes);
  WeightedAStar.print_node solution SlidingTile.print_tile_puzzle SlidingTile.string_of_action

let run_dps_gw (conf) = 
  run (conf);
  let t = Sys.time () in
  let (solution,nodes) = run_gridworld_dps (conf) in
  print_endline (Printf.sprintf "Execution time: %fs\n" (Sys.time () -. t));
  print_endline (Printf.sprintf "Nodes Expanded: %d\n" nodes);
  DynPSearch.print_node solution GridWorld.print_state GridWorld.string_of_action

let run_dps_stp (conf) = 
  run (conf);
  let t = Sys.time () in
  let (solution, nodes) = run_tiles_dps (conf) in
  print_endline (Printf.sprintf "Execution time: %fs\n" (Sys.time () -. t));
  print_endline (Printf.sprintf "Nodes Expanded %d\n" nodes);
  DynPSearch.print_node solution SlidingTile.print_tile_puzzle SlidingTile.string_of_action




