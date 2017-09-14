open UnionFind
open Earley

(** Signature for atomic type see type.mli *)
module type Atom = sig
  type t
  val eq : t -> t -> bool
  val print : Format.formatter -> t -> unit
  val parse : t grammar
end

(** Signature of the module returned by the function below see type.mli *)
module type Type = sig
  type atom

  type t' = private
          | Atom of atom
          | Func of t * t

   and t = t' UnionFind.t

  val atom : atom -> t
  val func : t -> t -> t
  val mkvar : string -> t

  exception Clash of t * t

  val unif : t -> t -> unit

  val print : Format.formatter -> t -> unit

  val parse : t Earley.grammar

  exception Cyclic of t

  val free_vars : t -> t list

  type schema

  val instanciate : schema -> t

  val generalise : t -> schema

  val print_schema : Format.formatter -> schema -> unit
end

module Make(A:Atom) = struct
  type atom = A.t

  type t' =
    | Atom of A.t
    | Func of t * t

   and t = t' UnionFind.t

  (** constructing type using the UnionFind constructor *)
  let atom : A.t -> t = fun a -> make_valued (Atom a)
  let func : t -> t -> t = fun t1 t2 -> make_valued (Func(t1,t2))
  let mkvar : string -> t = make_free

  (** Exception when unification fails *)
  exception Clash of t * t

  let clash t1 t2 = raise (Clash(make_valued t1, make_valued t2))

  (** unification: very short, as most of the job is done by UnionFind *)
  let rec unif : t -> t -> unit =
    let merge t1 t2 =
      match (t1, t2) with
      | (Atom a1, Atom a2) when A.eq a1 a2 -> ()
      | (Func(t1l, t1r), Func(t2l, t2r)) -> unif t1l t2l; unif t1r t2r
      | _ -> clash t1 t2
    in
    fun t1 t2 -> union t1 t2 merge

  (** priority levels *)
  type lvl = LvlAtom | LvlFunc

  (** Printing with minimum number of parenthesis *)
  (* TODO: use Format and add break hints *)
  let print : Format.formatter -> t -> unit =
    fun ff t ->
      let open UnionFind in
      let gn mark unmark =
        let rec fn wrap ff t =
          let (t0, v) = find t in
          match mark t0 with
          | Above -> Format.fprintf ff "CYCLE"
          | _ ->
             begin
               match v with
               | Unknown name -> Format.fprintf ff "?%s" name
               | Known(Atom a) -> Format.fprintf ff "%a" A.print a
               | Known(Func(t1,t2)) ->
                  let op, cl = if wrap = LvlAtom then "(", ")" else "", "" in
                  Format.fprintf ff "%s%a -> %a%s"
                                 op (fn LvlAtom) t1 (fn LvlFunc) t2 cl
             end;
             unmark t0
        in fn LvlFunc ff t
      in
      traverse gn

  (** parser for types, with the same priority *)
  let parser parse lvl =
    | a:A.parse
         when lvl = LvlAtom -> atom a
    | t1:(parse LvlAtom) "->" t2:(parse LvlFunc)
         when lvl = LvlFunc ->
         func t1 t2
    | "(" t:(parse LvlFunc) ")"
         when lvl = LvlAtom -> t
    | t:(parse LvlAtom) when lvl = LvlFunc -> t

  let parse = parse LvlFunc

  (** We use bindlib module to define schema *)
  open Bindlib

  type schema = (t, t) mbinder

  let instanciate : schema -> t = fun schema ->
    snd (unmbind (fun v -> mkvar (name_of v)) schema)

  exception Cyclic of t

  let free_vars : t -> t list = fun t0 ->
    let fn mark unmark =
      let l = ref [] in
      let rec gn t1 =
        let (v, t) = UnionFind.find t1 in
        match mark v with
        | Above -> raise (Cyclic t1)
        | Adone -> ()
        | _ ->
           begin
             match t with
             | Known (Func(t1,t2)) -> gn t2; gn t1
             | Known (Atom _) -> ()
             | Unknown _ -> l := v ::!l
           end;
           unmark v
      in
      gn t0; !l
    in
    traverse fn

  let bfunc = box_apply2 func
  let batom a = box (atom a)

  let generalise : t -> schema = fun t ->
    let vars = free_vars t in
    let map = List.map
                 (fun v ->
                   let name = match find v with
                     | _, Unknown n -> n
                     | _ -> assert false
                   in
                   v, new_var (fun _ -> assert false) name)
                 vars
    in
    let vars = Array.of_list (List.map snd map) in
    let rec fn t =
      let (v, t) = find t in
      match t with
      | Known (Atom a) -> batom a
      | Known (Func(t1,t2)) -> bfunc (fn t1) (fn t2)
      | Unknown _ ->
         try box_of_var (List.assq v map) with Not_found -> assert false
    in
    unbox (bind_mvar vars (fn t))

  let print_schema ff sch = print ff (instanciate sch)

end
