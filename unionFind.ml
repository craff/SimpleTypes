type mark = Above | Adone | NotSeen

type 'a t =
  { name : string; mutable value : 'a link; mutable mark : mark }

 and 'a link =
   | Leaf
   | Link of 'a t
   | Final of 'a

type 'a value =
  | Unknown of string
  | Known of 'a

let make_valued v = { name = ""; value = Final v; mark = NotSeen }

let make_free name = { name; value = Leaf; mark = NotSeen }

let rec find : 'a t -> 'a t * 'a value = fun c ->
  match c.value with
  | Leaf -> (c, Unknown c.name)
  | Link c1 -> let (c2, _) as res = find c1 in
               if c1 != c2 then c.value <- Link c2;
               res
  | Final a -> (c, Known a)

let rec union : type a.a t -> a t -> (a -> a -> unit) -> unit =
  fun c1 c2 merge ->
    let c1, a1 = find c1 in
    let c2, a2 = find c2 in
    if c1 != c2 then
      begin
        match (a1, a2) with
        | (Known a1, Known a2) ->
           c1.value <- Link c2; merge a1 a2
        | (Known a1, Unknown _) ->
           c2.value <- Link c1
        | _ ->
           c1.value <- Link c2
      end

let traverse fn =
  let marked = ref [] in
  let mark t =
    match t.mark with
    | Above | Adone as r -> r
    | NotSeen ->
       begin
         t.mark <- Above;
         marked := t :: !marked;
         NotSeen
      end
  in
  let unmark t = t.mark <- Adone in
  let res = fn mark unmark in
  List.iter (fun c -> c.mark <- NotSeen) !marked;
  res
