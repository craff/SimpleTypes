open SimpleType
open Bindlib

(** Module type for a signature for the simply typed lambda calculus.
    That is a type of all constants their types *)
module type Signature =
  sig
    module Type:Type
    type cst
    val typeOf : cst -> Type.t
    val print : Format.formatter -> cst -> unit
    val parse : cst Earley.grammar
  end

module type Term = sig
  module S:Signature
  module Type:Type

  type term =
    | Cst of S.cst
    | Lam of (term, term) binder
    | App of term * term
    | Def of definition

    | Var of term var (** for printing only *)
    | Typ of Type.t   (** for typing only *)
  and definition =
    { name : string
    ; schema : (Type.t, Type.t) mbinder
    ; value : term }

  (** Smart constructor for bindlibs *)
  val var : term var -> term

  val lam : string -> (term bindbox -> term bindbox) -> term bindbox

  val app : term bindbox -> term bindbox -> term bindbox

  (** type inferrence *)
  val infer : term -> Type.schema

  (** Printing and parsing *)
  val print : Format.formatter -> term -> unit

  val parse : term Earley.grammar
end

module Make(S:Signature) : Term with module S = S and module Type = S.Type
