let ( << ) = Fun.compose

let ( >> ) f g x = g (f x)

(* Sieve of Eratosthenes *)
let primes =
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
    | None ->
        Stream.Cons (n, lazy (map |> M.add (n * n) n |> primes_from (n + 1)))
    | Some p -> map |> M.remove n |> M.safe_add (n + p) p |> primes_from (n + 1)
  in
  primes_from 2 M.empty
  |> Stream.drop_while (fun p -> p < 10000)
  |> Stream.take_while (fun p -> p <= 99999)

(* Given p3, find all solutions (p1, p2, p3) *)
let solve primes p3 =
  let p3_digits = Digit.to_digits p3 in
  (*
    n = current digit index
    p1s = remaining candidates for p1
    p2s = remaining candidates for p2
    digits = remaining digits (haven't been used by either p1 or p2)
  *)
  let rec aux (n, p1s, p2s, digits) =
    if Stream.is_empty p1s || Stream.is_empty p2s then fun _ -> Stream.Empty
    else
      let extract_digits n = List.take n << Stream.first ~default:[] in
      if
        (* Invalid solutions *)
        let p1_digits = extract_digits n p1s in
        let p2_digits = extract_digits n p2s in
        p1_digits > p2_digits || p1_digits = p3_digits || p2_digits = p3_digits
      then fun _ -> Stream.Empty
      else
        let narrow = Stream.filter << List.match_index n in
        function
        | d :: ds when List.mem d digits ->
            let digits = List.remove d digits in
            (* For each digit of p3, either p1 or p2 matches it.
            hits = remaining primes for the one that matches
            misses = remaining primes for the other      
          *)
            let branch pair hits misses =
              let hits = narrow d hits in
              digits |> Stream.to_stream
              |> Stream.flatmap (fun d' ->
                  let misses = narrow d' misses in
                  let digits = List.remove d' digits in
                  let p1s, p2s = pair hits misses in
                  aux (n + 1, p1s, p2s, digits) ds)
            in
            (* Generate solutions for both cases, then concatenate *)
            Stream.cat
              (branch (fun x y -> (x, y)) p1s p2s)
              (branch (fun x y -> (y, x)) p2s p1s)
        | [] -> (
            match (p1s, p2s) with
            | Stream.Cons (p1_digits, _), Stream.Cons (p2_digits, _) ->
                let p1 = Digit.to_int p1_digits
                and p2 = Digit.to_int p2_digits in
                Stream.singleton (p1, p2, p3)
            | _ -> Stream.Empty)
        (* When running out of digits *)
        | _ :: _ -> Stream.Empty
  in
  let candidates = primes |> Stream.to_stream |> Stream.map Digit.to_digits in
  aux (0, candidates, candidates, Digit.all) p3_digits

let () =
  let out_file = "solutions.txt" in
  let oc = open_out out_file in

  let prime_list = Stream.to_list primes in
  print_endline "Calculating...";
  let result =
    let open Parallel in
    parallelize 8 prime_list (fun on_progress ->
        Stream.to_stream
        >> Stream.flatmap (solve prime_list)
        >> Stream.map (fun (p1, p2, p3) ->
            Printf.fprintf oc "(%d, %d, %d)\n" p1 p2 p3;
            on_progress p3)
        >> Stream.count)
    |> List.fold_left ( + ) 0
  in

  close_out oc;
  Printf.printf "Solutions: %d\n" result;
  Printf.printf "Output written to %s\n" out_file
