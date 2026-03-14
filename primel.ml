let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

module List = struct
  include Stdlib.List

  let remove v = filter (( <> ) v)

  let rec modify i f = function
    | [] -> failwith "Index error"
    | x :: xs when i = 0 -> f x :: xs
    | x :: xs -> x :: modify (i - 1) f xs

  let rec has_dup = function
    | [] -> false
    | x :: xs -> mem x xs || has_dup xs

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
end

module Digits = struct
  let rec to_digits = function
    | 0 -> []
    | n -> (n mod 10) :: to_digits (n / 10)

  let all = List.init 10 Fun.id

  module T = struct
    type t = Node of t list | Leaf of bool

    let empty = Leaf false

    let rec insert = function
      | 0 -> fun _ -> Leaf true
      | n -> (
          let idx = n mod 10 in
          let insert_next = insert (n / 10) in
          function
          | Node children -> Node (List.modify idx insert_next children)
          | _ ->
              Node
                (List.init 10 (fun i ->
                     if i = idx then insert_next (Leaf true) else Leaf false)))

    let from_list = List.fold_left (Fun.flip insert) empty

    let narrow d = function
      | Node children -> List.nth children d
      | t -> t
  end
end

(* Sieve of Eratosthenes *)
let primes =
  (* Mapping composites to smallest prime factor *)
  let open Map.Make (Int) in
  let safe_add n p map =
    (* If a number is already crossed out, try the next multiple *)
    let rec handle_collision n =
      if mem n map then handle_collision (n + p) else n
    in
    add (handle_collision n) p map
  in
  let rec from n map =
    match find_opt n map with
    (* Prime -> cross out n^2 *)
    | None -> Seq.Cons (n, fun () -> map |> add (n * n) n |> from (n + 1))
    (* Not prime -> cross out next multiple *)
    | Some p -> map |> remove n |> safe_add (n + p) p |> from (n + 1)
  in

  (fun () -> from 2 empty)
  |> Seq.drop_while (( > ) 10000)
  |> Seq.filter (not << List.has_dup << Digits.to_digits)
  |> Seq.take_while (( > ) 99999)
  |> List.of_seq

let primes_trie = Digits.T.from_list primes

(* Given p3, find all solutions (p1, p2, p3) *)
let solve p3 =
  (*
    rem_p1, rem_p2 = remaining candidates for p1, p2
    acc_p1, acc_p2 = current p1, p2 (building digit-by-digit right-to-left)
    digits = remaining unused digits
    n = 10^(current index)
    p = current p3 (removing digit-by-digit right-to-left) *)
  let rec aux (rem_p1, rem_p2, acc_p1, acc_p2, rem_digits, n, p) =
    if rem_p1 = Digits.T.empty || rem_p2 = Digits.T.empty then []
    else if p = 0 then
      if acc_p1 < acc_p2 && acc_p1 <> p3 && acc_p2 <> p3 then
        [ (acc_p1, acc_p2, p3) ]
      else []
    else
      let d = p mod 10 in
      (* Running out of digits *)
      if (not << List.mem d) rem_digits then []
      else
        let rem_digits = List.remove d rem_digits in
        (* For each digit of p3, either p1 or p2 matches it.
        hit = the one that matches
        miss = the one that doesn't *)
        let branch arrange (rem_hit, acc_hit) (rem_miss, acc_miss) =
          let hit = (Digits.T.narrow d rem_hit, acc_hit + (n * d)) in
          rem_digits
          |> List.concat_map (fun d' ->
              let rem_digits = List.remove d' rem_digits in
              let miss = (Digits.T.narrow d' rem_miss, acc_miss + (n * d')) in
              let (rem_p1, acc_p1), (rem_p2, acc_p2) = arrange hit miss in
              aux (rem_p1, rem_p2, acc_p1, acc_p2, rem_digits, n * 10, p / 10))
        in
        (* Solve both branches and concatenate *)
        let p1 = (rem_p1, acc_p1) and p2 = (rem_p2, acc_p2) in
        branch (fun x y -> (x, y)) p1 p2 @ branch (fun x y -> (y, x)) p2 p1
  in
  aux (primes_trie, primes_trie, 0, 0, Digits.all, 1, p3)

let parallelize count f domain =
  List.(
    domain
    |> to_chunks ((length domain + count - 1) / count)
    |> map (fun chunk -> Domain.spawn (fun () -> f chunk))
    |> map Domain.join)

let () =
  let out_file = "solutions.txt" in
  let oc = open_out out_file in
  let m = Mutex.create () in
  let on_progress count (p1, p2, p3) =
    Mutex.lock m;
    Printf.fprintf oc "(%d, %d, %d)\n" p1 p2 p3;
    Mutex.unlock m;
    count + 1
  in
  let solution_count =
    primes
    |> parallelize 12 (List.concat_map solve >> List.fold_left on_progress 0)
    |> List.fold_left ( + ) 0
  in
  close_out oc;
  Printf.printf "Solutions: %d\n" solution_count;
  Printf.printf "Output written to %s\n" out_file
