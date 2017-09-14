(** This module provide a functor building types and function
    for simply typed lambda calculus, it supports ML style polymorphism
    for global definition.

    It provides type-checking, convertibility, weak-head normal form
    printing and parsing and extesion with global definitions *)

open SimpleType
open Bindlib

(** Module type for a signature for the constants you want to
    use in your simply typed lambda-calculus. *)
module type Signature =
  sig
    (** We include a type module to have access to the type
        of simple type *)
    module Type:Type
    (** This is the type of the constants *)
    type t
    (** The equality, printing and parsing *)
    val eq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val parse : t Earley.grammar
    (** The function returning the type of a constant *)
    val typeOf : t -> Type.t
  end

(** Signature of simple terms *)
module type Term = sig
  (** We need to module for the signature and the types to write
      the type below *)
  module S:Signature
  module Type:Type

  (* The type of terms *)
  type t =
    | Cst of S.t
    | Lam of (t, t) binder
    | App of t * t
    | Def of definition

    | Var of t var (** for printing only *)
    | Typ of Type.t   (** for typing only *)
  and definition =
    { name : string
    ; schema : Type.schema
    ; value : t }

  (** Smart constructor for bindlibs *)
  val var : t var -> t

  val lam : string -> (t bindbox -> t bindbox) -> t bindbox

  val app : t bindbox -> t bindbox -> t bindbox

  val def : definition -> t bindbox

  (** type inferrence, of checking if the optional
      argument is given *)
  val infer : ?ty:Type.t -> t -> Type.schema

  (** infer and instanciate *)
  val typeOf : t -> Type.t

  (** Printing and parsing *)
  val print : Format.formatter -> t -> unit

  (** Exception raised by parsing when an unbound variable is encounterd *)
  exception Unbound of string

  val parse : t Earley.grammar

  (** We export the grammar for identifiers *)
  val lid : string Earley.grammar

  (** weak head normal form *)
  val whnf : t -> t

  (** convertibility test *)
  val eq : t -> t -> bool

  (** Adding a definition, possibly with a type. The term is
      type-checked and the definition is added to a global table used
      by parsing. *)
  val add_def : t -> ?ty:Type.t -> string -> definition

  (** Printing of definitions *)
  val print_def : Format.formatter -> definition -> unit
end

module Make(S:Signature) : Term with module S = S and module Type = S.Type
