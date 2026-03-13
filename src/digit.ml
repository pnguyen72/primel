let rec to_digits = function
  | 0 -> Stream.Empty
  | n -> Cons (n mod 10, lazy (to_digits (n / 10)))

let all = List.init 10 Fun.id |> Stream.to_stream

module T = struct
  type t = Node of t list | Leaf | Nil

  let rec insert = function
    | 0 -> fun _ -> Leaf
    | n -> (
        let idx = n mod 10 in
        let insert_next = insert (n / 10) in
        function
        | Node children -> Node (List.update idx insert_next children)
        | _ ->
            Node
              (List.init 10 (fun i -> if i = idx then insert_next Leaf else Nil))
        )

  let narrow d = function
    | Node children -> List.nth children d
    | t -> t
end
