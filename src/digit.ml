let rec to_digits = function
  | 0 -> []
  | n -> to_digits (n / 10) @ [ n mod 10 ]

let to_int = List.fold_left (fun acc d -> (acc * 10) + d) 0

let all = List.init 10 Fun.id |> Stream.to_stream
