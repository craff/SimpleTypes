open Type
open Simple

module Atom = struct
  type t = Nat
  let eq = (=)
  let print ff Nat = Format.fprintf ff "Nat"
  let parser parse = "Nat" -> Nat
end

module Type = Type.Make(Atom)

open Atom
open Type

module Sig = struct
  module T = Type
  type cst = Zero | Succ
  let typeOf = function
    | Zero -> atom Nat
    | Succ -> func (atom Nat) (atom Nat)
  let print ff = function
    | Zero -> Format.fprintf ff "0"
    | Succ -> Format.fprintf ff "S"
  let parser parse =
    | "0" -> Zero
    | "S" -> Succ
end

module Term = Simple.Make(Type)(Sig)

let blank = Earley.blank_regexp ''[ \t\n\r]*''
let test =
  let idt = Earley.parse_string Term.parse blank "fun x -> x" in
  let ty = Term.infer idt in
  Format.printf "%a\n" Type.print_schema ty;
  let succ = Earley.parse_string Term.parse blank "fun n f x -> f (n f x)" in
  let ty = Term.infer succ in
  Format.printf "%a\n" Type.print_schema ty;
  let trois = Earley.parse_string Term.parse blank "fun f x -> f (f (f x))" in
  let ty = Term.infer trois in
  Format.printf "%a\n" Type.print_schema ty;
  let ty = Term.infer (Term.App(trois, trois)) in
  Format.printf "%a\n" Type.print_schema ty
