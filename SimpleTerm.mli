open SimpleType
open Bindlib

(** Module type for a signature for the simply typed lambda calculus.
    That is a type of all constants their types *)
module type Signature =
  sig
    module Type:Type
    type t
    val eq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val parse : t Earley.grammar
    val typeOf : t -> Type.t
  end

module type Term = sig
  module S:Signature
  module Type:Type

  type term =
    | Cst of S.t
    | Lam of (term, term) binder
    | App of term * term
    | Def of definition

    | Var of term var (** for printing only *)
    | Typ of Type.t   (** for typing only *)
  and definition =
    { name : string
    ; schema : Type.schema
    ; value : term }
  type t = term

  (** Smart constructor for bindlibs *)
  val var : term var -> term

  val lam : string -> (term bindbox -> term bindbox) -> term bindbox

  val app : term bindbox -> term bindbox -> term bindbox

  val def : definition -> term bindbox

  (** type inferrence, of checking if the optional
      argument is given *)
  val infer : ?ty:Type.t -> term -> Type.schema

  (** infer and instanciate *)
  val typeOf : term -> Type.t

  (** Printing and parsing *)
  val print : Format.formatter -> term -> unit

  val parse : term Earley.grammar

  (** weak head normal form *)
  val whnf : term -> term

  (** convertibility test *)
  val eq : term -> term -> bool

  (** Adding a definition, possibly with a type *)
  val add_def : term -> ?ty:Type.t -> string -> unit
end

module Make(S:Signature) : Term with module S = S and module Type = S.Type
