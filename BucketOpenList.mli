exception BucketOpenListError of string

type ghPair

type 'a bucket 

type 'a t 

val create :
  'a ->
  float ->
  ('a -> int -> unit) ->
  ('a -> float) -> ('a -> float) -> ('a -> int) -> ('a -> string) -> 'a t

val insert : 'a t -> 'a -> unit

val checkFMin : 'a t -> bool

val fixOpenList : 'a t -> unit

val isNotEmpty : 'a t -> bool

val replace : 'a t -> 'a -> ('a -> string) -> unit

val chooseNode : 'a t -> 'a

val print : 'a t -> ('a -> string) -> unit

val verify : 'a t -> unit
