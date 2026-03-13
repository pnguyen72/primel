type 'a t = Cons of 'a * 'a t Lazy.t | Empty

let ( !! ) = Lazy.force

let first ~default = function
  | Cons (item, _) -> item
  | _ -> default

let rec to_stream = function
  | [] -> Empty
  | x :: xs -> Cons (x, lazy (to_stream xs))

let to_list s =
  let rec aux acc = function
    | Cons (item, next) -> aux (item :: acc) !!next
    | _ -> List.rev acc
  in
  aux [] s

let singleton v = Cons (v, lazy Empty)

let rec fold_left f acc = function
  | Cons (item, next) -> fold_left f (f acc item) !!next
  | Empty -> acc

let rec drop_while p = function
  | Cons (item, next) when p item -> drop_while p !!next
  | s -> s

let rec take_while p = function
  | Cons (item, next) when p item -> Cons (item, lazy (take_while p !!next))
  | _ -> Empty

let rec map f = function
  | Cons (item, next) -> Cons (f item, lazy (map f !!next))
  | _ -> Empty

let rec filter p = function
  | Cons (item, next) when p item -> Cons (item, lazy (filter p !!next))
  | Cons (_, next) -> filter p !!next
  | _ -> Empty

let remove v = filter (( <> ) v)

let rec mem v = function
  | Cons (v', _) when v = v' -> true
  | Cons (_, next) -> mem v !!next
  | _ -> false

let rec has_dup = function
  | Empty -> false
  | Cons (item, next) -> mem item !!next || has_dup !!next

let rec cat s1 s2 =
  match s1 with
  | Cons (item, next) -> Cons (item, lazy (cat !!next s2))
  | _ -> !!s2

let rec flatmap f = function
  | Cons (item, next) -> cat (f item) (lazy (flatmap f !!next))
  | _ -> Empty
