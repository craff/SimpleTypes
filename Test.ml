open SimpleType
open SimpleTerm

(* Example: simply typed terms with natural numbers,
   would require some form of rewriting to be usefull *)
module Atom = struct
  type t = Nat
  let eq = (=)
  let print ff Nat = Format.fprintf ff "Nat"
  let parser parse = "Nat" -> Nat
end

module Type = SimpleType.Make(Atom)

open Atom
open Type

module Sig = struct
  module Type = Type
  type t = Zero | Succ
  let typeOf = function
    | Zero -> atom Nat
    | Succ -> func (atom Nat) (atom Nat)
  let print ff = function
    | Zero -> Format.fprintf ff "0"
    | Succ -> Format.fprintf ff "S"
  let parser parse =
    | "0" -> Zero
    | "S" -> Succ
  let eq = (=)
end

module Term = SimpleTerm.Make(Sig)

let blank = Earley.blank_regexp ''[ \t\n\r]*''
let test =
  let idt = Earley.parse_string Term.parse blank "fun x -> x" in
  let ty = Term.infer idt in
  Format.printf "%a : %a\n" Term.print idt Type.print_schema ty;
  let succ = Earley.parse_string Term.parse blank "fun n f x -> f (n f x)" in
  let ty = Term.infer succ in
  Format.printf "%a : %a\n" Term.print succ Type.print_schema ty;
  let trois = Earley.parse_string Term.parse blank "fun f x -> f (f (f x))" in
  let ty = Term.infer trois in
  Format.printf "%a : %a\n" Term.print trois Type.print_schema ty;
  let neuf = Term.App(trois, trois) in
  let ty = Term.infer neuf in
  Format.printf "%a : %a\n" Term.print neuf Type.print_schema ty;
  let trois' = Earley.parse_string Term.parse blank "S (S (S 0))" in
  let ty = Term.infer trois' in
  Format.printf "%a : %a\n" Term.print trois' Type.print_schema ty
