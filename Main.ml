(*Main driver for the suboptimality code*)

let rec read_lines () =
  try let line = read_line () in
    (line::[]) :: read_lines()
  with 
    End_of_file -> []

(*
 * Assume the file is set up so that:
 * domain - gw 
 * algorithm - wa*
 * height - int
 * width - int
 * grid height x width field 
 * - '#' - obstacle
 * - '@' - agent
 * - '*' - goal
 *)

let () = 
  let domain : string = read_line () in
  let algorithm : string = read_line () in
  let bound : string = read_line () in
  let height : string  = read_line () in
  let width : string = read_line () in
  let grid : string list list = read_lines () in
  let conf = Executive.{
      domain = domain;
      algorithm = algorithm;
      bound = bound;
      height = height;
      width = width;
      problem = grid
    } in
  let to_run =  (algorithm, domain) in
  match to_run with
  | ("wa*", "gw") -> Executive.run_wa_gw (conf)
  | ("wa*", "stp") -> Executive.run_wa_stp (conf)
  | ("dps", "gw") -> Executive.run_dps_gw(conf)
  | ("dps", "stp") -> Executive.run_dps_stp (conf)
  | _ -> raise (Executive.ExecutiveError "unimplemented configuration")

