open SimpleType
open Bindlib

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

    | Var of term var (** for printing and convertibility only *)
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

module Make(S:Signature) = struct
  module S = S
  module Type = S.Type

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

  let var : term var -> term =
    fun v -> Var v

  let lam : string -> (term bindbox -> term bindbox) -> term bindbox =
    fun name f ->
      box_apply (fun f -> Lam f) (bind var name f)

  let app : term bindbox -> term bindbox -> term bindbox =
    box_apply2 (fun t1 t2 -> App(t1,t2))

  let def : definition -> term bindbox =
    fun d -> box (Def d)

  let infer : ?ty:Type.t -> term -> Type.schema = fun ?ty term ->
    let open Type in
    let gen_var =
      let count = ref 0 in
      (fun () ->
        let c = !count in
        count := c + 1;
        string_of_int c)
    in
    let new_type () = mkvar (gen_var ()) in
    let rec fn : term -> Type.t -> unit =
      fun term typ ->
        match term with
        | Cst c -> unif (S.typeOf c) typ
        | Lam f ->
           let ty_arg = new_type () in
           let ty_res = new_type () in
           let (_, t1) = unbind (fun _ -> Typ ty_arg) f in
           unif typ (func ty_arg ty_res);
           fn t1 ty_res
        | Typ t -> unif t typ
        | App(t1,t2) ->
           let ty_arg = new_type () in
           fn t1 (func ty_arg typ);
           fn t2 ty_arg
        | Def d ->
           let t = instanciate d.schema in
           unif t typ
        | Var _ -> assert false
    in
    let ty = match ty with
      | None -> new_type ()
      | Some ty -> ty
    in
    fn term ty;
    generalise ty

  type lvl = LvlAtom | LvlApp | LvlLam

  let rec print_ids ff = function
    | [] -> ()
    | [v] -> Format.fprintf ff "%s" (name_of v)
    | v::l -> Format.fprintf ff "%s %a" (name_of v) print_ids l

  let rec print lvl ff term =
    match term with
    | Cst c -> S.print ff c
    | Def d -> Format.fprintf ff "%s" d.name
    | Var v -> Format.fprintf ff "%s" (name_of v)
    | App(t,u) ->
       let op, cl = if lvl < LvlApp then "(", ")" else "", "" in
       Format.fprintf ff "%s%a %a%s" op (print LvlApp) t (print LvlAtom) u cl
    | Lam(_) ->
       let op, cl = if lvl < LvlLam then "(", ")" else "", "" in
       let rec fn acc t = match t with
           Lam(f) ->
           let (v,t) = unbind var f in
           fn (v::acc) t
         | u ->
            let ids = List.rev acc in
            Format.fprintf ff "%sfun %a -> %a%s"
                           op print_ids ids (print LvlLam) t cl
       in
       fn [] term
    | Typ _ -> assert false

  let print = print LvlLam

  exception Unbound of string
  let unbound v = raise (Unbound v)

  let global_defs = Hashtbl.create 101

  let add_def term ?ty name =
    let schema = infer ?ty term in
    let def = { name; value = term; schema } in
    Hashtbl.add global_defs name def

  let parser lid = ''[a-z][a-zA-Z0-9_']*''
  let parser parse lvl =
    | c : S.parse
         when lvl = LvlAtom -> (fun env -> box (Cst c))

    | v:lid
         when lvl = LvlAtom ->
              (fun env ->
                try List.assoc v env with Not_found ->
                try def (Hashtbl.find global_defs v) with Not_found -> unbound v)

    | "fun" ids:lid* "->" t:(parse LvlLam)
         when lvl = LvlLam ->
       List.fold_right (fun v t env ->
           lam v (fun x -> let env = (v,x)::env in t env)) ids t

    | t:(parse LvlAtom) u:(parse LvlApp)
        when lvl = LvlApp ->
             (fun env -> app (t env) (u env))

    | "(" t:(parse LvlLam) ")"
         when lvl = LvlAtom -> t

    | t:(parse LvlAtom) when lvl = LvlApp -> t
    | t:(parse LvlApp) when lvl = LvlLam -> t

  let parse = parser t:(parse LvlLam) -> unbox (t [])

  let rec whnf : term -> term =
    fun t ->
      match t with
      | App(u,v) | Def { value = App(u,v) } ->
         begin
           match whnf u with
           | Lam f | Def { value = Lam f } -> subst f v
           | u' -> if u == u' then t (* do not open definition for nothing *)
                   else App(u',v)
         end
      | _ -> t

  let rec eq : term -> term -> bool =
    fun t1 t2 ->
      if t1 == t2 then true else
        (* use fn when we know terms are in whnf *)
        let rec fn t1 t2 =
          match t1, t2 with
          | Def(d1), Def(d2) when d1 == d2 -> true
          | Def(d1), _ -> fn d1.value t2
          | _, Def(d2) -> fn t1 d2.value
          | App(u1,v1), App(u2,v2) -> fn u1 u2 && eq v1 v2
          | Lam(f1), Lam(f2) ->
             let (x,u1) = unbind var f1 in
             let u2 = subst f1 (Var x) in
             eq u1 u2
          | Var(v1), Var(v2) -> eq_vars v1 v2
          | Cst(c1), Cst(c2) -> S.eq c1 c2
          | _ -> false
        in fn (whnf t1) (whnf t2)

  let typeOf t = Type.instanciate (infer t)


end
