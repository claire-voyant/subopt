type ('state, 'action) node 

val print_node : ('a, 'b) node -> ('a -> string) -> ('b -> string) -> unit

val node_notify : 'a -> 'b -> unit

val index_f : ('a, 'b) node -> int -> unit

val get_i : ('a, 'b) node -> int

val get_f : ('a, 'b) node -> float

val get_g : ('a, 'b) node -> float

val addToExisting :
  ('a, 'b) node BucketOpenList.t ->
  ('a, ('a, 'b) node) Hashtbl.t ->
  float ->
  float -> 'a -> 'b -> 'b list -> ExperimentRecord.node_stats -> unit

val evaluateSuccessor :
  ('a -> float) ->
  ('a, 'b) node BucketOpenList.t ->
  ('a, ('a, 'b) node) Hashtbl.t ->
  'a ->
  'b -> float -> ('a, 'b) node -> ExperimentRecord.node_stats -> unit

val expandCurrentNode :
  ('a, 'b) node BucketOpenList.t ->
  ExperimentRecord.node_stats ->
  ('a -> bool) ->
  ('a -> float) ->
  ('a, ('a, 'b) node) Hashtbl.t ->
  ('a -> ('a * 'b * float) list) ->
  ('a, 'b) node * int

val createStructures :
  float ->
  'a ->
  ('a, 'b) node ->
  ('a, 'b) node BucketOpenList.t * ('a, ('a, 'b) node) Hashtbl.t * ExperimentRecord.node_stats

val run :
  'a ->
  ('a -> bool) ->
  ('a -> ('a * 'b * float) list) ->
  ('a -> float) -> float -> ('a, 'b) node * int
