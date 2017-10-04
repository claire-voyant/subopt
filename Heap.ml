(*Implementation of a Heap*)

type 'a heap =
  | Leaf
  | Node of 'a heap * 'a * 'a heap * int

let max_heap_size = 10000000;;

let singleton k = Node (Leaf, k, Leaf, 1)

let rank = function Leaf -> 0 | Node(_,_,_,r) -> r

let rec merge t1 t2 =
  match t1, t2 with
    | Leaf, t | t, Leaf -> t
    | Node(l, k1, r, _), Node(_, k2, _, _) ->
      if k1 > k2 then merge t2 t1
      else
        let merged = merge r t2 in
        let rank_left = rank l and rank_right = rank merged in
        if rank_left >= rank_right then Node(l, k1, merged, rank_right+1)
        else Node (merged, k1, l, rank_left+1)

let insert x t = merge (singleton x) t

let get_min = function
  | Leaf -> failwith "empty"
  | Node (_, k, _, _) -> k

let delete_min = function
  | Leaf -> failwith "empty"
  | Node (l, _, r, _) -> merge l r

let print_heap f h =
  let copy = ref h in
  for i=0 to max_heap_size do
    try print_string ((f (get_min !copy)) ^ "\n");
        copy := delete_min !copy;
    with
      Failure("empty") -> print_string ""
  done;
  print_string "\n";;
