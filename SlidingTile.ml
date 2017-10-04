(* SlidingTile implementation 15 puzzle *)

(* SlidingTile structures and exception *)

exception SlidingTileError of string

type state = {
  board:int array;
  mutable blank:int;
}

type action = North | South | East | West

type t = {
  tiles : int;
  bound : float;
  width : int;
  height : int;
}

let print_tile_puzzle state = 
  let s = ref "" in
  let p = state.board in
  for j=0 to 15 do
    if j mod 4 = 0 then
      s := !s ^ "\n";
    s := !s ^ (Printf.sprintf "%d\t" p.(j));
  done;
  s := !s ^ "\n" ^ "\tblank: " ^ (Printf.sprintf "%d\n" state.blank);
  !(s)

let solution_fifteen = [|0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15|]

let in_same_range at be =
  if (0 <= at && at <= 3 && 0 <= be && be <= 3) then
    true
  else if (4 <= at && at <= 7 && 4 <= be && be <= 7) then
    true
  else if (8 <= at && at <= 11 && 8 <= be && be <= 11) then
    true
  else if (12 <= at && at <= 15 && 12 <= be && be <= 15) then
    true
  else
    false

(* how many tiles out of place from solution *)

let heuristic domain state = 
  let bound = domain.bound in
    let h_val = ref 0.0 in
    Array.iteri (fun i tile ->
      let at = ref i in 
      let be = ref tile in
      let is_correct = (tile = solution_fifteen.(i)) in
      if not is_correct then
        (
          let rec loop () =
            if (not (in_same_range !at !be)) then
              (
              if !be < !at then
                (
                  at := !at - 4;
                  h_val := !h_val +. 1.0;
                  (*print_string "+"; *)
                  loop ();
                )
              else
                (
                  at := !at + 4;
                  h_val := !h_val +. 1.0;
                  (* print_string "+";*)
                  loop ();
                )
            )
          in
          loop ();
          (* print_endline "";*)
          (* print_endline (Printf.sprintf "%d" (abs (!be - !at)));*)
          h_val := !h_val +. (float_of_int ((abs (!be - !at))));
        )
    ) state.board;
    (!h_val) *. bound

let heuristic_dps domain state = 
  let bound = domain.bound in
  let wrong_h = heuristic domain state in
  wrong_h /. bound

let make_empty_world tiles h w bound = {
  tiles = tiles;
  bound = bound;
  width = w;
  height = h; 
}

let is_goal domain state = 
  if domain.tiles = 16 then
    state.board = solution_fifteen
  else 
    false

let string_of_action a =
  match a with
  | North -> "N"
  | East -> "E"
  | South -> "S"
  | West -> "W"

let kid allow s b =
  if allow then
    let copy_board = Array.copy s.board in
    let new_kid = {board=copy_board;blank=s.blank} in
    new_kid.board.(s.blank) <- s.board.(b);
    new_kid.blank <- b;
    new_kid.board.(b) <- 0;
    new_kid
  else
    s

let successors domain s =
 (* print_endline "expanding:";
  print_endline (print_tile_puzzle s);*)
  let width = domain.width in
  let n_tiles = domain.tiles in
  let c_action = [((s.blank >= width),North,(kid (s.blank >= width) s (s.blank - width)));
                  (((s.blank mod width) < (width - 1)),East,(kid ((s.blank mod width) < (width - 1)) s (s.blank + 1))); 
                  (((s.blank mod width) > 0),West,(kid ((s.blank mod width) > 0) s (s.blank - 1))); 
                  ((s.blank < (n_tiles - width)),South, (kid (s.blank < (n_tiles - width)) s (s.blank + width)))] in
  let ss = ref [] in
  List.iter (fun (allowed, action, successor) ->
      if allowed then
        (
          (*print_endline "successor:"; 
          print_endline (print_tile_puzzle successor); *)
          ss := (successor, action, 1.0) :: !ss
        )
    ) c_action;
  !(ss)

