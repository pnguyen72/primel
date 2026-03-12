type 'a t = Cons of 'a * 'a t Lazy.t | Empty

let first ~default = function
  | Cons (item, _) -> item
  | _ -> default

let is_empty t = t = Empty

let rec to_stream = function
  | [] -> Empty
  | x :: xs -> Cons (x, lazy (to_stream xs))

let to_list s =
  let rec aux acc = function
    | Cons (item, next) -> aux (item :: acc) (Lazy.force next)
    | _ -> List.rev acc
  in
  aux [] s

let singleton v = Cons (v, lazy Empty)

let count s =
  let rec aux acc = function
    | Cons (_, next) -> aux (acc + 1) (Lazy.force next)
    | _ -> acc
  in
  aux 0 s

let rec drop_while p = function
  | Cons (item, next) when p item -> drop_while p (Lazy.force next)
  | s -> s

let rec take_while p = function
  | Cons (item, next) when p item ->
      Cons (item, lazy (take_while p (Lazy.force next)))
  | _ -> Empty

let rec map f = function
  | Cons (item, next) -> Cons (f item, lazy (map f (Lazy.force next)))
  | _ -> Empty

let rec filter p = function
  | Cons (item, next) when p item ->
      Cons (item, lazy (filter p (Lazy.force next)))
  | Cons (_, next) -> filter p (Lazy.force next)
  | _ -> Empty

let remove v = filter (( <> ) v)

let rec mem v = function
  | Cons (v', _) when v = v' -> true
  | Cons (_, next) -> mem v (Lazy.force next)
  | _ -> false

let rec cat s1 s2 =
  match s1 with
  | Cons (item, next) -> Cons (item, lazy (cat (Lazy.force next) s2))
  | _ -> s2

let rec lazy_cat s1 s2 =
  match s1 with
  | Cons (item, next) -> Cons (item, lazy (lazy_cat (Lazy.force next) s2))
  | _ -> Lazy.force s2

let rec flatmap f = function
  | Cons (item, next) -> lazy_cat (f item) (lazy (flatmap f (Lazy.force next)))
  | _ -> Empty
