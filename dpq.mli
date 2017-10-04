(* $Id: dpq.mli,v 1.2 2004/08/19 17:35:52 ruml Exp ruml $
   
   simple destructive priority queues via a binary heap

   in the style of Sedgewick's code in "Algorithms in C"
*)

(* NOTE: this is mostly the same as dpq in the structs system.

   Differences:
     - re-sort the queue by a new predicate
     - remove does both push up and pull down
*)

type 'a t


(** basic interface **)

(* constants *)
val no_position : int
  
(*val create_with : ('a -> 'a -> bool) -> 'a -> 'a t*)
  (** a simpler variant of [create] that doesn't require an update notifier
    or initial size *)

(* val from_array : ('a -> 'a -> bool) -> 'a array -> 'a t
  (** takes an array of initial contents.  doesn't require an update
    notifier or initial size.  faster than creating an empty queue and
    repeatedly calling insert *) *)
  
val set_garbage : 'a t -> 'a -> unit
  (** under usual operation, initial element will never be garbage
    collected. *)
  
val insert : 'a t -> 'a -> unit
  
val count : 'a t -> int
  (** number of objects in q.  constant time.  *)

val clear : 'a t -> unit
  (** remove all objects *)

val resort : 'a t -> ('a -> 'a -> bool) -> unit
  (** re-sort the queue using a new predicate. *)

val empty_p : 'a t -> bool
  
exception Empty of string
  
val extract_first : 'a t -> 'a
  (** removes and returns an object at the beginning of the ordering
      defined by the queue's predicate.  may raise Empty *)

val peek_first : 'a t -> 'a
  (** returns but does not remove the object at the beginning of the
      ordering defined by the queue's predicate.  may raise Empty *)

val iter : ('a -> unit) -> 'a t -> unit
  (** order is arbitrary.  tolerates modifications.  iterates over elements
      present at time of call. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** order is arbitrary.  tolerates modifications.  iterates over elements
      present at time of call. *)

val iter_unsafe : ('a -> bool) -> 'a t -> unit
  (** order is arbitrary.  stops early if function returns false.
      modifications of the set during the iterating may cause elements to be
      repeated or skipped *)

(*val make_iterator_unsafe : 'a t -> (unit -> 'a option) * (unit -> unit)*)
  (** returns a function that returns elements in best-first order and a
      function that resets the iterator to the beginning.  All bets are off if
      queue is modified during iterating (OK just before reset, though). *)
  
  
(** location aware interface *****)

  
val create : ('a -> 'a -> bool) ->  ('a -> int -> unit) -> int -> 'a -> 'a t
  (** takes predicate, update notifier, initial size, garbage element.
      predicate should be true iff elements are in the desired order, including
      equality!  Notified is called whenever an object is placed into a
      location in the underlying heap.  If the caller tracks this information,
      then the see_update and remove functions can be used.  initial size can
      be just a guess. garbage element is not added to the q.  Note: this
      garbage element will never be garbage-collected unless another
      never-to-be-collected element is specified via set_garbage.  Sorry, but
      array must be filled with something! *)

val get_at : 'a t -> int -> 'a

val see_update : 'a t -> int -> unit
  (** ordering of object at given location may have changed *)

val remove : 'a t -> int -> unit
  (** remove object at given location *)

val swap : 'a t -> int -> 'a -> unit
  (** remove object at location and add new object.  it is not assumed that
      the new object has the same priority as the old.  probably slightly
      faster than removing and then inserting.  will call notifier to set pos
      of new object. *)

  
(** outside of usual interface *****)

val check_heap : 'a t -> unit

val check_index : ('a -> int) -> 'a t -> unit

val check_index2 : ('a -> int) -> 'a t -> unit

val check : ('a -> int) -> 'a t -> unit
  
(*val test1 : bool -> int -> unit*)
  

(* EOF *)
