(*let test_create_bucket_open _ =
  let bop = BucketOpenList.create BucketOpenList.sort_b Data.garb_notify 5 99.0 BucketOpenList.garb_bucket 1.0 in
  print_endline (Printf.sprintf "create_bucket_open\n");
  BucketOpenList.print bop; 
  let empty_gw = GridWorld.empty_world 3 3 in
  let successors = GridWorld.successors {x=0;y=0} empty_gw in
  print_endline (Printf.sprintf "gibt es %d Nachfolgerin" (List.length successors));
  let j = ref 0.0 in
  print_endline (Printf.sprintf "inserting...");
  for i=1 to (List.length successors) do
    let sa = List.nth successors (i-1) in
    let n = Data.new_node !j 0.0 (!j +. 1.0) sa.s sa.a None in
    BucketOpenList.insert bop n;
    BucketOpenList.print bop;
    j := !j +. 1.0;
    print_endline (Printf.sprintf "-----next-------");
  done;
  print_endline (Printf.sprintf "removing...");
  for i=1 to (List.length successors) do
    let sa = List.nth successors (i-1) in
    let n = Data.new_node !j 0.0 (!j +. 1.0) sa.s sa.a None in
    let r = BucketOpenList.remove bop in 
    Data.print_node r;
    BucketOpenList.print bop;
    j := !j +. 1.0;
    print_endline (Printf.sprintf "-----next-------");
  done;
  let r = BucketOpenList.remove bop in
  Data.print_node r;
  BucketOpenList.print bop; 
  print_endline (Printf.sprintf "-----finally-------");
  ;; 
*) 

type m_tup = {
  mutable i : int;
  mutable f : float;
  mutable g : float;
}

let print_m_tup n =
  print_endline (Printf.sprintf "i:%d | f:%f | g:%f\n" n.i n.f n.g)

let p_tup n =
  (Printf.sprintf "i:%d | f:%f | g:%f\n" n.i n.f n.g)

let i_f n i = n.i <- i

let get_f n = n.f

let get_g n = n.g

let get_i n = n.i

let test_check_bucket_list () = 
  let init_node = {i=0;f=0.0;g=0.0;} in
  let openlist = BucketOpenList.create init_node 1.0 i_f get_f get_g get_i print_m_tup in
  let closedlist = Hashtbl.create 100 in
  BucketOpenList.insert openlist init_node;
  Hashtbl.add closedlist init_node init_node;
  try
    for i=0 to 9 do
      let new_node = {i=i;f=(float_of_int i);g=((-1.0) *. (float_of_int i));} in
      print_m_tup new_node;
      for i=0 to 2 do 
        BucketOpenList.insert openlist new_node;
        Hashtbl.add closedlist new_node new_node;
      done;
    done;
    for i=0 to 9 do
      for i=0 to 2 do
        let dummy = BucketOpenList.chooseNode openlist in ()
      done;
    done;
    print_endline "last check";
    BucketOpenList.verify openlist;
    BucketOpenList.print openlist p_tup;
  with _ -> 
    print_endline "failure!";
    BucketOpenList.print openlist p_tup


let () = test_check_bucket_list ()

