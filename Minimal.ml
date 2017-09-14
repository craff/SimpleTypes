(** An interpretor for the minimal application of the provided functors,
    No constant, no atomic type (still not empty as we have ML polymorphism).

Here is a session example:

{v
  >>> def idt = fun x -> x;
  idt = fun x -> x : ?2 -> ?2
  >>> whnf idt idt;
  idt
  >>> test idt idt = idt ;
  idt idt = idt => true
  >>> def z = fun f x -> x;
  z = fun f x -> x : ?1 -> ?4 -> ?4
  >>> def s1 = fun n f x -> f (n f x);
  s1 = fun n f x -> f (n f x) : ((?7 -> ?6) -> ?8 -> ?7) -> (?7 -> ?6) -> ?8 -> ?6
  >>> def s2 = fun n f x -> n f (f x);
  s2 = fun n f x -> n f (f x) : ((?9 -> ?7) -> ?7 -> ?6) -> (?9 -> ?7) -> ?9 -> ?6
  >>> test (s1 (s1 z)) = (s2 (s2 z));
  s1 (s1 z) = s2 (s2 z) => true
  >>> ^D Bye
v}
*)

(** Empty atomic module *)
module Atom = struct
  type t = { nothing : 'a.'a } (* simplest definition of the empty type ? *)
  let eq = (=)
  let print ff x = assert false
  let parse : t Earley.grammar = Earley.fail () (* grammar parsing nothing *)
end

(** The type with no atom: remark type of closed type
    is empty, but the type of schema is not *)
module Type = SimpleType.Make(Atom)

open Atom
open Type

(** The empty signature *)
module Sig = struct
  include Atom (* Yes it is the same *)
  module Type = Type
  let typeOf x = assert false
end

(** The module for terms *)
module Term = SimpleTerm.Make(Sig)

(** The blanks *)
let blank = Earley.blank_regexp ''[ \t\n\r]*''

let quit () = Format.printf "Bye\n%!"; exit 0

(** The command. Earley do not execute them until ";" is reached *)
let parser command =
  | "def" name:Term.lid ty:{ _:":" Type.parse }? "=" term:Term.parse ->
     (fun () ->
       let def = Term.add_def term ?ty name in
       Format.printf "%a\n%!" Term.print_def def)
  | "whnf" term:Term.parse ->
     (fun () ->
       Format.printf "%a\n%!" Term.print (Term.whnf term))
  | "test" t1:Term.parse "=" t2:Term.parse ->
     (fun () ->
       Format.printf "%a = %a => %b\n%!" Term.print t1 Term.print t2
                     (Term.eq t1 t2))
  | EOF -> quit ()
  | { "exit" | "quit" } -> quit

(** Earley trick to make sure we read line by line *)
let parser full_command =
  f:(Earley.change_layout command blank) ";" -> f ()

(** The main loop, with exception handling *)
let main () =
  while true do
    try
      Format.printf ">>> %!";
      let stdbuf = Input.from_channel stdin in
      ignore (Earley.(partial_parse_buffer full_command no_blank stdbuf 0));
    with
    | Type.Cyclic t -> Format.eprintf "Cyclic type: %a\n%!" Type.print t
    | Type.Clash(t1,t2) -> assert false (* not possible here *)
    | Term.Unbound n -> Format.eprintf "Unbound variable %S\n%!" n
    | Earley.Parse_error(buf,pos,msgs) ->
       let open Input in
       Printf.eprintf "File %S, line %d, character %d:\n%!"
                      (filename buf) (line_num buf) (utf8_col_num buf pos);
       Printf.eprintf "Parse error:\n%!";
       List.iter (Printf.eprintf " - %s\n%!") msgs
  done

let _ = main ()
