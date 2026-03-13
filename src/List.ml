include Stdlib.List

let rec last = function
  | [] -> failwith "Empty list"
  | [ x ] -> x
  | _ :: xs -> last xs

let rec match_index idx v = function
  | x :: _ when idx = 0 -> x = v
  | _ :: xs -> match_index (idx - 1) v xs
  | [] -> false

let to_chunks n =
  let rec split chunk rem list =
    match (rem, list) with
    | 0, _ | _, [] -> (rev chunk, list)
    | _, x :: xs -> split (x :: chunk) (rem - 1) xs
  in
  let rec collect chunks = function
    | [] -> rev chunks
    | list ->
        let chunk, rest = split [] n list in
        collect (chunk :: chunks) rest
  in
  if n < 1 then failwith "Chunk size must be positive" else collect []
