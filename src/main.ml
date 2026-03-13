open Stream

let ( << ) = Fun.compose

(* Sieve of Eratosthenes *)
let generate_primes =
  let module M = struct
    include Map.Make (Int)

    let safe_add n p map =
      let rec handle_collision n =
        if mem n map then handle_collision (n + p) else n
      in
      add (handle_collision n) p map
  end in
  let rec primes_from n map =
    match M.find_opt n map with
    | None -> Cons (n, lazy (map |> M.add (n * n) n |> primes_from (n + 1)))
    | Some p -> map |> M.remove n |> M.safe_add (n + p) p |> primes_from (n + 1)
  in
  primes_from 2 M.empty

(* Given p3, find all solutions (p1, p2, p3) *)
let solve candidates p3 =
  let p3_digits = Digit.to_digits p3 in
  (*
    n = current digit index
    p1s = remaining candidates for p1
    p2s = remaining candidates for p2
    digits = remaining digits (haven't been used by either p1 or p2) *)
  let rec aux (n, p1s, p2s, digits) =
    if p1s = Empty || p2s = Empty then fun _ -> Empty
    else if
      (* Invalid solutions *)
      let get_digits = List.take n << first ~default:[] in
      let p1_digits = get_digits p1s and p2_digits = get_digits p2s in
      p1_digits > p2_digits || p1_digits = p3_digits || p2_digits = p3_digits
    then fun _ -> Empty
    else function
      (* Out of digits *)
      | d :: _ when (not << mem d) digits -> Empty
      | d :: ds -> (
          match remove d digits with
          (* Out of digits *)
          | Empty -> Empty
          | digits ->
              let narrow_candidates = filter << List.match_index n in
              (* For each digit of p3, either p1 or p2 matches it.
                hits = remaining primes for the one that matches
                misses = remaining primes for the other *)
              let branch pair hits misses =
                let hits = hits |> narrow_candidates d in
                flatmap
                  (fun d' ->
                    let misses = misses |> narrow_candidates d' in
                    let digits = digits |> remove d' in
                    let p1s, p2s = pair hits misses in
                    aux (n + 1, p1s, p2s, digits) ds)
                  digits
              in
              (* Generate solutions for both cases, then concatenate *)
              cat
                (branch (fun x y -> (x, y)) p1s p2s)
                (lazy (branch (fun x y -> (y, x)) p2s p1s)))
      | [] -> (
          match (p1s, p2s) with
          | Cons (p1_digits, _), Cons (p2_digits, _) ->
              let p1 = Digit.to_int p1_digits and p2 = Digit.to_int p2_digits in
              singleton (p1, p2, p3)
          | _ -> Empty)
  in
  aux (0, candidates, candidates, Digit.all) p3_digits

let () =
  let out_file = "solutions.txt" in
  let oc = open_out out_file in
  let on_progress callback count (p1, p2, p3) =
    Printf.fprintf oc "(%d, %d, %d)\n" p1 p2 p3;
    callback p3;
    count + 1
  in

  let primes =
    generate_primes
    |> drop_while (( > ) 10000)
    |> take_while (( > ) 99999)
    |> filter (not << List.has_dup << Digit.to_digits)
    |> to_list
  in
  let result =
    List.fold_left ( + ) 0
      (Parallel.parallelize 12
         (fun callback ->
           fold_left (on_progress callback) 0
           << (flatmap << solve << map Digit.to_digits << to_stream) primes
           << to_stream)
         primes)
  in

  close_out oc;
  Printf.printf "Solutions: %d\n" result;
  Printf.printf "Output written to %s\n" out_file
