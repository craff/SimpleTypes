(** Implementation of union find, with union
    supporting a call back *)

(** Type of a union find cell, the main type of this library.  The
    type parameter ['a] an optional information attached to each
    equavalence classes of ['a t] *)
type 'a t

(** [ make_valued a ] creates a new cell, alone within its equivalence class
    and with value a attached *)
val make_valued : 'a -> 'a t

(** [make_free name ] creates a new cell, alone within its equivalence class
    and  with no value attached *)
val make_free : string -> 'a t

(** The main function and its return type *)
type 'a value = Unknown of string | Known of 'a

(** [ find a ] returns the element of type [ 'a t ] canonically
   representing the equivalent class of [ a ]. This means that [ a1 ]
   and [ a2 ] are equivalent if and only if
     [ fst (find a1) == fst (find a2) ].

   [ find a ] also returns the value attached to the equivalence class
   of a if any, or the name of the cell if none. *)
val find : 'a t -> 'a t * 'a value

(** [ union a1 a2 merge ] merges the equivalent classes of [ a1 ] and
    [ a2 ] and calls [ merge v1 v2 ] if [ a1 ] and [ a2 ] where
    distinct and attached to the value [ v1 ] and [ v2 ]
    respectively. *)
val union : 'a t -> 'a t -> ('a -> 'a -> unit) -> unit

(** [ traverse fn ] will return [ fn mark unmark ] when mark is a
    function you can use to test and mark a value of type [ 'a t ].
    [ mark v ] returns [ Above ] if [ v ] was previously marked and
    [ unmark v ] was not called and [ Adone ] otherwise.  If [ v ] is
    not marked, it returns [ NotSeen ] and marks [ v ]. *)
type mark = Above | Adone | NotSeen
val traverse : (('a t -> mark) -> ('a t -> unit) -> 'b) -> 'b
