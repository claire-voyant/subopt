(* Bucket based open list for DynPSearch *)

exception BucketOpenListError of string

type ghPair = {
  mutable g : float;
  mutable h : float;
}

type 'a bucket = {
  mutable f : float;
  mutable g : float;
  mutable h : float;
  mutable free : int;
  mutable nodes : 'a array;
}

type 'a t = {
  bound : float;
  mutable fmin : float;
  openlist : ('a  bucket) Dpq.t;
  lookup : (ghPair, ('a bucket)) Hashtbl.t;
  garb_ele : 'a;
  set_index : 'a -> int -> unit;
  get_f : 'a -> float;
  get_g : 'a -> float;
  get_i : 'a -> int;
  printer : 'a -> string;
}

let print_bucket (b: 'a bucket) p_node = 
  print_endline (Printf.sprintf "---\n");
  print_endline (Printf.sprintf "f: %f g: %f h: %f" b.f b.g b.h);
  print_endline (Printf.sprintf "BucketNodeArray %d" (Array.length b.nodes));
  Array.iter (fun a -> print_string (p_node a)) b.nodes;
  print_endline (Printf.sprintf "\nFree slot %d" b.free);
  print_endline (Printf.sprintf "---\n")

let print q p_node =
  print_endline (Printf.sprintf "Fmin: %f..." (q.fmin));
  print_endline (Printf.sprintf "OpenList size: %d..." (Dpq.count q.openlist));
  Dpq.iter (fun n -> print_bucket n p_node) q.openlist;
  print_endline (Printf.sprintf "---\n");
  print_endline (Printf.sprintf "BucketLookUp size: %d..." (Hashtbl.length q.lookup));
  Hashtbl.iter (fun k n -> print_bucket n p_node) q.lookup

let check_fmin q verify bf node = 
  if (q.get_f node < q.fmin) then verify := false

let verifyFVals q = 
  let verified = ref true in
  let fmin = q.fmin in
  let bucket_table = q.lookup in
  let check_compare_f (pair:ghPair) bucket = 
    let bucket_f = pair.g +. pair.h in
    let nodes = bucket.nodes in
    if (bucket.free <> 0) then (
      for i=0 to bucket.free-1 do
        if (q.get_f bucket.nodes.(i) <> bucket.f) then (
          verified := false;
          print_bucket bucket q.printer;
        )
      done;
    )
  in
  Hashtbl.iter check_compare_f bucket_table;
  !verified

let verifyNodes q f = 
  let verified = ref true in
  let fmin = q.fmin in
  let bucket_table = q.lookup in
  let check_compare_f (pair:ghPair) bucket = 
    let bucket_f = pair.g +. pair.h in
    let nodes = bucket.nodes in
    let check_node = (fun node -> f q verified bucket_f node) in
    Array.iter check_node nodes
  in
  Hashtbl.iter check_compare_f bucket_table;
  !verified

let verify q str = 
  (*print_endline (Printf.sprintf "%s" str);*)
  let a = verifyNodes q check_fmin in
  if (not a) then (
    failwith "check_fmin failed";
  );
  let a = verifyFVals q in
  if (not a) then (
    failwith "verifying bucket match nodes failed";
  )


let garb_bucket garb_node = {
  f = (-1.0);
  g = (-1.0);
  h = (-1.0);
  free = (-1);
  nodes = Array.make 1 garb_node
}

let sort_b q a_bucket b_bucket =
  let a_ud = ((q.bound *. q.fmin) -. a_bucket.g) /. (a_bucket.h) in
  let b_ud = ((q.bound *. q.fmin) -. b_bucket.g) /. (b_bucket.h) in
  (* print_endline (Printf.sprintf "a_ud: %f b_ud %f" a_ud b_ud); *)
  if b_ud < a_ud then 
    true
  else
    false

let create node init_bound index_f get_f get_g get_i print = 
  let init_fmin = infinity in
  let garb_b = garb_bucket node in
  let garb_update (a: 'a bucket) n = () in
  let garb_sort (a:'a bucket) (b:'b bucket) = true in
  let approx_n = max 1000000 8 in 
  let heap = Dpq.create garb_sort garb_update approx_n garb_b in
  let table = Hashtbl.create approx_n in 
  let garb_pair = {g = (-1.0); h = (-1.0)} in
  Hashtbl.add table garb_pair garb_b;
  Hashtbl.clear table;
  let bop = {
    bound = init_bound;
    fmin = init_fmin;
    openlist = heap;
    lookup = table;
    garb_ele = garb_b.nodes.(0);
    set_index = index_f;
    get_f = get_f;
    get_g = get_g;
    get_i = get_i;
    printer = print;
  } in
  let sorter = (fun a b -> sort_b bop a b) in
  Dpq.resort bop.openlist sorter;
  bop

let empty q = Dpq.empty_p q.openlist

(*
  when inserting into our openlist heap
  fmin could have changed check if new element
  being inserted has a lower f value when the
  current fmin - if it does update fmin
*)
let insert q e =
  (*verify q "verifying beginning of insert";*)
  (*print_endline (Printf.sprintf "time to insert!");*)
  (*print_endline (Printf.sprintf "e.f < fmin | %f < %f" (q.get_f e) q.fmin);*)
  if (q.get_f e) < q.fmin then
    (
      (* 
      if the node being inserted has a lower f value
      then we may not have a bucket for that f value  
      check if we have one in the lookup table otherwise
      create one and place it into the table and openlist 
    *)
      verify q "first step of insert";
      q.fmin <- q.get_f e; 
      let gh_pair = {g = q.get_g e; h = ((q.get_f e) -. (q.get_g e))} in
      if (Hashtbl.mem q.lookup gh_pair) then
        (
      (*
        we have a gh_pair in the hashtable the f value of
        e is lower than fmin so we need to take this bucket
        which should have an empty nodes array and place
        the new node into the array and put the bucket into
        the openlist heap
      *)
          let known_empty_bucket = Hashtbl.find q.lookup gh_pair in
          if (known_empty_bucket.free = 0) then
            (
              (* 
          make certain that the bucket is empty otherwise
          fail with an exception as something went wrong
        *)
              (*print_endline (Printf.sprintf "empty bucket with fmin change" );*)
              known_empty_bucket.nodes.(0) <- e;
              known_empty_bucket.free <- 1;
              q.set_index e 0;
              Dpq.insert q.openlist known_empty_bucket; 
            )
            (*else
              (
                raise (BucketOpenListError "BucketOpenList.insert e.f < fmin and 
                            the bucket in the table was not empty")
              )
            *)
        )
      else
        (
      (*
        if there is no bucket we need to make one and add it 
        into the openlist as well as the lookup hashtable
      *) 
          (*print_endline (Printf.sprintf "no bucket with fmin change");*)
          let new_bucket_array = Array.make 1 q.garb_ele in 
          new_bucket_array.(0) <- e;
          q.set_index e 0;
          let new_bucket = {
            f = q.get_f e;
            g = q.get_g e;
            h = (q.get_f e) -. (q.get_g e);
            free = 1;
            nodes = new_bucket_array; 
          } in 
          let new_bucket_pair = {g = q.get_g e; h = ((q.get_f e) -. (q.get_g e))} in
          Hashtbl.add q.lookup new_bucket_pair new_bucket;
          Dpq.insert q.openlist new_bucket;
        )
    )
  else
    (
      verify q "second step of insert";
      (* 
      our fmin value is not changing but we need to find
      the bucket the node will be placed into similar to before
    *)
      let gh_pair = {g = q.get_g e; h = ((q.get_f e) -. (q.get_g e))} in
      if (Hashtbl.mem q.lookup gh_pair) then
        (
      (*
        our bucket here isn't necessarily empty as fmin 
        did not change but it can be so we do similar checks
      *)
          verify q "found the gh-pair";
          let some_bucket = Hashtbl.find q.lookup gh_pair in

          (*print_endline (Printf.sprintf "%d" (Array.length some_bucket.nodes));*)
          if (some_bucket.free = 0) then
            (
              (*print_endline (Printf.sprintf "empty bucket with no fmin change");*)
              some_bucket.nodes.(0) <- e;
              some_bucket.free <- 1;
              q.set_index e 0;
              Dpq.insert q.openlist some_bucket;
            ) 
          else
            (
        (*
          here the array list isn't empty so we go to
          first free slot and add the new node in its not
          empty so it must be in the open list already  
          be certain to update the indices correctly of free
          and the one of where the node is in the bucket nodes array 
        *)

              (*print_endline (Printf.sprintf "%B" (some_bucket.free >= (Array.length some_bucket.nodes)));*)
              if (some_bucket.free >= (Array.length some_bucket.nodes)) then
                (
		 
	          verify q "bucket is not empty and we ran out of free space";
          (*
            no more space in nodes array but need to add another node
            expand by times 2 the size of the nodes array
          *)  
                  (* print_endline (Printf.sprintf "no space with no fmin change");*)
                  let append_array = Array.make (some_bucket.free*2) q.garb_ele in
                  let new_nodes_array = Array.append some_bucket.nodes append_array in
                  some_bucket.nodes <- new_nodes_array;
                  some_bucket.nodes.(some_bucket.free) <- e; 
                  q.set_index e some_bucket.free;
                  some_bucket.free <- some_bucket.free + 1; 
                )
              else
                (
          (*
           * there is space so just add it and update free appropriately
           *
           *)
	          verify q "bucket is not empty and we have enough free space";
		  q.printer e;
                  (*print_endline (Printf.sprintf "space with no fmin change");*)
                  some_bucket.nodes.(some_bucket.free) <- e; 
                  q.set_index e some_bucket.free;
                  some_bucket.free <- some_bucket.free + 1;
                )
            )
        )
      else
        (
          (* 
        the node wasn't in the lookup hashtable and fmin isn't changing
        just create a new bucket and place the node into the array list
        placing the bucket into the lookup hashtable and the openlist
      *)
          (*print_endline (Printf.sprintf "new node and no fmin change");*)
          let new_bucket_array = Array.make 1 q.garb_ele in 
          new_bucket_array.(0) <- e;
          q.set_index e 0;
          let new_bucket = {
            f = q.get_f e;
            g = q.get_g e;
            h = (q.get_f e) -. (q.get_g e);
            free = 1;
            nodes = new_bucket_array; 
          } in 
          let new_bucket_pair = {g = q.get_g e; h = ((q.get_f e) -. (q.get_g e))} in
          Hashtbl.add q.lookup new_bucket_pair new_bucket;
          Dpq.insert q.openlist new_bucket;

        )
    )
(*verify q;*)
(*print_endline (Printf.sprintf "end to insert!");*)

let add q e = insert q e

let checkFMin q =
  let fmin_ok = ref infinity in
  let cur_fmin = q.fmin in
  let check_fmin a = if a.f < !fmin_ok then fmin_ok := a.f in
  Dpq.iter check_fmin q.openlist;
  if !fmin_ok < cur_fmin then
    (
      q.fmin <- !fmin_ok;
      false
    )
  else
    true

let fixOpenList q = Dpq.resort q.openlist (sort_b q)

let findFMin q = q.fmin

let isNotEmpty q =
  let len = Dpq.count q.openlist in
  if (len = 0) then
    false
  else
    true

let checkFmin q = 
  let fmin_ok = ref infinity in
  let check_fmin a = if a.f < !fmin_ok then fmin_ok := a.f in
  Dpq.iter check_fmin q.openlist; 
  !fmin_ok

let print_meta e node_gh p_node q = 
  print_endline (Printf.sprintf "time to replace!");
  print q p_node;
  print_endline (Printf.sprintf "index: %d" (q.get_i e));
  print_endline (Printf.sprintf "new_f: %f | new_g: %f" (q.get_f e) (q.get_g e));
  print_endline (Printf.sprintf "bucket_f %f" (Hashtbl.find q.lookup node_gh).f);
  print_bucket (Hashtbl.find q.lookup node_gh) p_node;
  print_endline (Printf.sprintf "bucket_size: %d" (Array.length (Hashtbl.find q.lookup node_gh).nodes))

let replace q e replacement p_node =
  verify q "verifying beginning of replace";
  insert q replacement
(*  let node_gh = {g = q.get_g e; h = ((q.get_f e) -. (q.get_g e))} in
  try 
    let bucket_lookup = Hashtbl.find q.lookup node_gh in
    (*print_endline (Printf.sprintf "free: %d" (bucket_lookup.free));*)
    if ((q.get_i e >= 0) && (bucket_lookup.free > 0)) then 
      let the_replacement = bucket_lookup.nodes.(bucket_lookup.free - 1) in
      bucket_lookup.nodes.(q.get_i e) <- the_replacement;
      bucket_lookup.free <- bucket_lookup.free - 1;
      q.set_index the_replacement (q.get_i e);
      insert q e;
  with 
    Not_found -> insert q e
  | Invalid_argument "index out of bounds" -> insert q e
*)
(*
  look at the top of the heap q.openlist
  and check the array use openlist peek
*)
let remove q =
  (*verify q;*)
  let left_to_remove = Dpq.count q.openlist in
  if (left_to_remove = 0) then
    raise (BucketOpenListError "BucketOpenList.remove nothing left to remove");
  let next_node = ref q.garb_ele in
  let openlist_top = Dpq.peek_first q.openlist in
  (*print_endline (Printf.sprintf "Bucket Array length %d" (Array.length openlist_top.nodes));
    print_endline (Printf.sprintf "Bucket free pointer %d" (openlist_top.free));*)
  if (openlist_top.free = 0) then
    (
    (*
      if there are no nodes left so we have  
      to update fmin and remove the bucket and
      reheapify on the new fmin value and pull
      from the next bucket after removing and 
      update the free index
    *) 
      let top_open = Dpq.extract_first q.openlist in
      let top_check = checkFmin q in
      let next_bucket = Dpq.peek_first q.openlist in
      let n_node = next_bucket.nodes.(next_bucket.free - 1) in
      if top_check < q.fmin then
        q.fmin <- top_check; 
      Dpq.resort q.openlist (sort_b q);
      next_bucket.free <- (next_bucket.free - 1);
      next_node := n_node;
      (*q.set_index !next_node (-1);*)

      (*verify q;*)
      !next_node;
    )
  else
    (
    (*
      there is at least one node that we can pull from
      get that node and update the free index, if there
      is exactly one node, free -> 1, then we have to remove
      the bucket from the open list as well otherwise we just
      remove the node from the bucket entirely
    *)   
      if (openlist_top.free = 1) then
        (
          let n_node = openlist_top.nodes.(openlist_top.free - 1) in
          openlist_top.free <- 0;
          next_node := n_node;
          let top_open = Dpq.extract_first q.openlist in
          let top_check = checkFmin q in
          if top_check < q.fmin then
            q.fmin <- top_check;
          Dpq.resort q.openlist (sort_b q); 
          (*q.set_index !next_node (-1);*)

          (*verify q;*)
          !next_node;
        )
      else
        ( 
          let n_node = openlist_top.nodes.(openlist_top.free - 1) in
          openlist_top.free <- openlist_top.free - 1;
          next_node := n_node;
          (*q.set_index !next_node (-1);*)

          (*verify q;*)
          !next_node;
        )
    )

let chooseNode q =
  let chosen_node = remove q in
  chosen_node


