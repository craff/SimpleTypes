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
  let idt_t = Earley.parse_string Term.parse blank "fun x -> x" in
  let idt = Term.add_def idt_t "idt" in
  Format.printf "%a\n" Term.print_def idt;
  let succ_t = Earley.parse_string Term.parse blank "fun n f x -> f (n f x)" in
  let succ = Term.add_def succ_t "succ" in
  Format.printf "%a\n" Term.print_def succ;
  let trois_t = Earley.parse_string Term.parse blank
                                  "succ (succ (succ (fun f x -> x)))"
  in
  let trois = Term.add_def trois_t "trois" in
  Format.printf "%a\n" Term.print_def trois;
  let neuf_t = Earley.parse_string Term.parse blank "trois trois" in
  let neuf = Term.add_def neuf_t "neuf" in
  Format.printf "%a\n" Term.print_def neuf;
  let trois_t' = Earley.parse_string Term.parse blank "S (S (S 0))" in
  let trois' = Term.add_def trois_t' "trois'" in
  Format.printf "%a\n" Term.print_def trois'
