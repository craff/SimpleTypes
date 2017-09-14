(** This module provide a functor SimpleType.Make to build simple types
    from atomic type.

    It provides their common functions (unification, printing,
    parsing, ...).

    It also provides schemas in the sense of ML polymorhism, with
    generalisation and instanciation *)

(** Signature for atomic type see type.mli.  We give a type for atomic
    type, their equality and printing and parsing function *)
module type Atom = sig
  type t
  val eq : t -> t -> bool
  val print : Format.formatter -> t -> unit
  val parse : t Earley.grammar
end

(** This is the signature of simple type. We use the UnionFind
    module to handle unification. *)
module type Type = sig
  type atom

  (** Constructors for types are private, to ensure that they are
      constructed via UnionFind module properly. You can still match
      them. *)
  type t' = private
          | Atom of atom
          | Func of t * t

   (** A type must therefore always be accessed via
       UnionFind.find, as UnionFind.t is Abstract *)
   and t = t' UnionFind.t

  (** Function to construct types *)
  val atom : atom -> t
  val func : t -> t -> t
  val mkvar : string -> t

  (** Exception raise by unification *)
  exception Clash of t * t

  (** Unification function, raise the exception when unification fails
      on a clash. The acyclicity test is not performed by this
      function. See below. *)
  val unif : t -> t -> unit

  (** Type printing. Will print "CYCLIC" where a cyclic types appears. *)
  val print : Format.formatter -> t -> unit

  (** Type parsing *)
  val parse : t Earley.grammar

  (** exceptino raised when cyclic (a.k.a. recursive) types are encountered *)
  exception Cyclic of t

  (** Compute the list of unbound type variable, without repetition,
      by order of appearance from left to right. May raise Cyclic. *)
  val free_vars : t -> t list

  (** Type of schema, that is a most general type, as in
      Hingley-Milner algorithm (i.e. like ML style polymorphism) *)
  type schema

  (** Instanciation of a schema using fresh unbound type variables *)
  val instanciate : schema -> t

  (** Build a schema by generalising all its type variables, may
      raise Cyclic too. *)
  val generalise : t -> schema

  (** Printing of schema *)
  val print_schema : Format.formatter -> schema -> unit
end

(** This main function will be the module for simple type
    from the definition of the atomic type *)
module Make(A:Atom) : Type with type atom = A.t
