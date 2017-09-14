
(** Signature for atomic type see type.mli *)
module type Atom = sig
  type t
  val eq : t -> t -> bool
  val print : Format.formatter -> t -> unit
  val parse : t Earley.grammar
end

module type Type = sig
  type atom

  (** thus constructors for types are private, to ensure that they are
      constructed via UnionFind module properly. You can still match
      them. *)

  type t' = private
          | Atom of atom
          | Func of t * t

   and t = t' UnionFind.t

  (** function to construct types *)
  val atom : atom -> t
  val func : t -> t -> t
  val mkvar : string -> t

  (** exception raise by unification *)
  exception Clash of t * t

  (** unification function *)
  val unif : t -> t -> unit

  (** type printing. Will print "CYCLIC" where a cyclic types appears. *)
  val print : Format.formatter -> t -> unit

  (** type parsing *)
  val parse : t Earley.grammar

  (** exceptino raised when cyclic (a.k.a. recursive) types are encountered *)
  exception Cyclic of t

  (** compute the list of unbound type variable, without repetition,
      by order of appearance from left to right. May raise Cyclic. *)
  val free_vars : t -> t list

  (** type type of schema as in Hingley-Milner algorithm *)
  type schema

  (** instanciation of a schema using fresh unbound type variables *)
  val instanciate : schema -> t

  (** build a schema by generalising all its type variables *)
  val generalise : t -> schema

  (** printing of schema *)
  val print_schema : Format.formatter -> schema -> unit
end

module Make(A:Atom) : Type with type atom = A.t
