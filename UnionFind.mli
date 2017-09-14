(** Implementation of union find, with union
    supporting a call back *)

(** Type of a union find cell, the main type of this library.  The
    type parameter ['a] an optional information attached to each
    equavalence classes of ['a t] *)
type 'a t

(** [ make_valued a ] creates a new cell, alone within its equivalence class
    and with value [ a ] attached *)
val make_valued : 'a -> 'a t

(** [make_free name ] creates a new cell, alone within its equivalence class
    and with no value attached *)
val make_free : string -> 'a t

(** The main function and its return type *)
type 'a value = Unknown of string | Known of 'a

(** [ find c ] returns a pair [ (c', a) ] where

   - [ c' ] is the element of type [ 'a t ] canonically representing
     the equivalent class of [ c ]. This means that [ c1 ] and [ c2 ]
     are equivalent if and only if [ fst (find c1) == fst (find c2) ].

   - [ a ] is the value attached to the equivalence class of a if any,
     or the name of the cell if none. *)
val find : 'a t -> 'a t * 'a value

(** [ union a1 a2 merge ] merges the equivalent classes of [ a1 ] and
    [ a2 ] and calls [ merge v1 v2 ] if [ a1 ] and [ a2 ] where
    distinct and attached to the value [ v1 ] and [ v2 ]
    respectively. *)
val union : 'a t -> 'a t -> ('a -> 'a -> unit) -> unit

(** [ traverse fn ] will return [ fn mark unmark ] where mark is a
    function you can use to test and mark a value of type [ 'a t ].
    [ mark v ] returns

    - [ Above ] if [ v ] was previously marked and [ unmark v ] was
      not called.
    - [ Adone ] if [ unmark v ] was called
    - [ NotSeen ] If [ v ] neither [ mark v ] nor [ unmark v ] where
      called.

    - mark and unmark are O(1) function to allow a non looping and linear
      time traversal of your union-find data structure. *)
type mark = Above | Adone | NotSeen
val traverse : (('a t -> mark) -> ('a t -> unit) -> 'b) -> 'b
