open Type
open Bindlib

module type Sig =
  sig
    module T:Type
    type cst
    val typeOf : cst -> T.t
    val print : Format.formatter -> cst -> unit
    val parse : cst Earley.grammar
  end

module Make(Type:Type)(S:Sig with module T = Type) = struct
  type term =
    | Cst of S.cst
    | Lam of (term, term) binder
    | App of (term * term)
    | Def of definition

    | Var of term var (** for printing only *)
    | Typ of Type.t   (** for typing only *)
  and definition =
    { name : string
    ; schema : (Type.t, Type.t) mbinder
    ; value : term }

  let var : term var -> term =
    fun _ -> failwith "Variable escaping its scope"

  let lam : string -> (term bindbox -> term bindbox) -> term bindbox =
    fun name f ->
      box_apply (fun f -> Lam f) (bind var name f)

  let app : term bindbox -> term bindbox -> term bindbox =
    box_apply2 (fun t1 t2 -> App(t1,t2))

  let infer : term -> Type.schema = fun term ->
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
           let (_, t) = unmbind (fun _ -> new_type ()) d.schema in
           unif t typ
        | Var _ -> assert false
    in
    let ty = new_type () in
    fn term ty;
    generalise ty

  type lvl = LvlAtom | LvlApp | LvlLam

  exception Unbound of string
  let unbound v = raise (Unbound v)

  let parser lid = ''[a-z][a-zA-Z0-9_']*''
  let parser parse lvl =
    | c : S.parse
         when lvl = LvlAtom -> (fun env -> box (Cst c))

    | v:lid
         when lvl = LvlAtom ->
              (fun env -> try List.assoc v env with Not_found -> unbound v)

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

end
