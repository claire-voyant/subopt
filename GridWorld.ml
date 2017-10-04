(*GridWorld implementation*)

(* GridWorld structures and exception *)

exception GridWorldError of string

type action = North | South | East | West 

type state = {
  x:int;
  y:int;
}

type t = {
  bound : float;
  grid : bool array array;
  width : int;
  height : int;
}

let heuristic domain state = 
  let h = domain.height in
  let w = domain.width in
  domain.bound *. (float_of_int (abs((w - 1) - state.x) + abs((h - 1) - state.y)))

let make_empty_world h w gw bound = {
  bound = bound;
  grid = gw;
  width = w;
  height = h;
}

let is_goal domain state = 
  state.x == (domain.width - 1) && state.y == (domain.height - 1)

let do_action state action =
  match action with
  | North -> {x=state.x;y=state.y-1}
  | East -> {x=state.x+1;y=state.y}
  | South -> {x=state.x;y=state.y+1}
  | West -> {x=state.x-1;y=state.y}

let string_of_action n =
  match n with
  | North -> "[N]"
  | East -> "[E]"
  | South -> "[S]"
  | West -> "[W]"

let print_state state =
  (Printf.sprintf "%d %d\n" state.x state.y)

let action_allowed s grid =
  let x_bounds = (s.x) >= 0 && (s.x) <= ((Array.length grid.(0))-1) in
  let y_bounds = (s.y) >= 0 && (s.y) <= ((Array.length grid)-1) in
  x_bounds && y_bounds && (not grid.(s.y).(s.x))

let successors domain state =
  (* print_endline "Expanding...";
  print_endline (print_state state); *)
  let actions = [(North, 1.0);(East, 1.0);(South, 1.0);(West, 1.0)] in
  let ss = ref [] in
  List.iter (fun (action, cost) ->
      let d_action = do_action state action in
      if action_allowed d_action domain.grid then
        (
          (* print_endline "Successor...";
          print_endline (print_state d_action);  *)
          ss := (d_action, action, cost) :: !ss
        )
    )
    actions;
  !(ss)


