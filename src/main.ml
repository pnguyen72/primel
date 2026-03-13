open Stream
open Digit

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let primes =
  let module M = struct
    include Map.Make (Int)

    let safe_add n p map =
      let rec handle_collision n =
        if mem n map then handle_collision (n + p) else n
      in
      add (handle_collision n) p map
  end in
  let rec from n map =
    match M.find_opt n map with
    | None -> Cons (n, lazy (map |> M.add (n * n) n |> from (n + 1)))
    | Some p -> map |> M.remove n |> M.safe_add (n + p) p |> from (n + 1)
  in
  from 2 M.empty
  |> drop_while (( > ) 10000)
  |> take_while (( > ) 99999)
  |> filter (not << has_dup << to_digits)

let primes_tree = fold_left (Fun.flip T.insert) T.Nil primes

(* Given p3, find all solutions (p1, p2, p3) *)
let solve p3 =
  (*
    n = 10^(current index)
    p1_rem, p2_rem = remaining candidates for p1, p2
    p1_acc, p2_acc = current p1, p2 (building digit-by-digit right-to-left)
    digits = remaining digits 
    p = current p3 (removing digit-by-digit right-to-left) *)
  let rec aux (n, p1_rem, p2_rem, p1_acc, p2_acc, digits, p) =
    if p1_rem = T.Nil || p2_rem = T.Nil then Empty
    else if p = 0 then
      if p1_acc < p2_acc && p1_acc <> p3 && p2_acc <> p3 then
        singleton (p1_acc, p2_acc, p3)
      else Empty
    else
      let d = p mod 10 in
      if (not << mem d) digits then Empty
      else
        let digits = remove d digits in
        let branch arrange (hit_rem, hit_acc) (miss_rem, miss_acc) =
          let hit = (T.narrow d hit_rem, hit_acc + (n * d)) in
          flatmap
            (fun d' ->
              let digits = remove d' digits in
              let miss = (T.narrow d' miss_rem, miss_acc + (n * d')) in
              let (p1_rem, p1_acc), (p2_rem, p2_acc) = arrange hit miss in
              aux (n * 10, p1_rem, p2_rem, p1_acc, p2_acc, digits, p / 10))
            digits
        in
        let p1 = (p1_rem, p1_acc) and p2 = (p2_rem, p2_acc) in
        cat
          (branch (fun x y -> (x, y)) p1 p2)
          (lazy (branch (fun x y -> (y, x)) p2 p1))
  in
  aux (1, primes_tree, primes_tree, 0, 0, all, p3)

let parallelize count f domain =
  domain
  |> List.to_chunks ((List.length domain + count - 1) / count)
  |> List.map (fun chunk -> Domain.spawn (fun () -> f chunk))
  |> List.map Domain.join

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
  let result =
    primes |> to_list (* Must convert to list to parallelize *)
    |> parallelize 12 (to_stream >> flatmap solve >> fold_left on_progress 0)
    |> List.fold_left ( + ) 0
  in
  close_out oc;
  Printf.printf "Solutions: %d\n" result;
  Printf.printf "Output written to %s\n" out_file
