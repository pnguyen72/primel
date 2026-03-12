(* ------ Helpers ------ *)

let ( << ) = Fun.compose
let rec to_digits = function 0 -> [] | n -> to_digits (n / 10) @ [ n mod 10 ]

let rec matches idx v = function
  | x :: _ when idx = 0 -> x = v
  | _ :: xs -> matches (idx - 1) v xs
  | [] -> false

(* Because there are only 10 digits, using List is actually faster than Set *)
let all_digits = List.init 10 Fun.id
let remove v = List.filter (( <> ) v)

let display_digits =
  print_endline << String.concat " " << List.map string_of_int

let display (p1, p2, p3) =
  display_digits p1;
  display_digits p2;
  print_endline (String.make 9 '-');
  display_digits p3;
  print_newline ()

(* ------ Main logic ------ *)

(* Sieve of Eratosthenes *)
let primes_generator =
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

(* 5-digit primes, each represented as a list of digits *)
let primes =
  primes_generator
  |> Stream.drop_while (fun p -> p < 10000)
  |> Stream.take_while (fun p -> p <= 99999)
  |> Stream.map to_digits

(* Given p3, find all solutions (p1, p2, p3) *)
let solve p3 =
  (*
    n = current digit index
    p1s = remaining candidates for p1
    p2s = remaining candidates for p2
    digits = remaining digits (haven't been used by either p1 or p2)
  *)
  let rec aux (n, p1s, p2s, digits) =
    let narrow = Stream.filter << matches n in
    let extract_digits n = List.take n << Stream.first ~default:[] in
    if
      (* Removing invalid solutions *)
      let p1 = extract_digits n p1s in
      let p2 = extract_digits n p2s in
      p1 > p2 || p1 = p3 || p2 = p3
    then fun _ -> Stream.Empty
    else function
      | d :: ds when List.mem d digits ->
          let digits = remove d digits in
          (* For each digit of p3, either p1 or p2 matches it.
            hit = remaining primes for the one that matches
            misses = remaining primes for the other      
          *)
          let branch pair hits misses =
            let hits = narrow d hits in
            digits |> Stream.to_stream
            |> Stream.flatmap (fun d' ->
                let misses = narrow d' misses in
                let digits = remove d' digits in
                let p1s, p2s = pair hits misses in
                aux (n + 1, p1s, p2s, digits) ds)
          in
          (* Generate solutions for both paths, then concatenate *)
          Stream.cat
            (branch (fun x y -> (x, y)) p1s p2s)
            (branch (fun x y -> (y, x)) p2s p1s)
      | [] -> (
          match (p1s, p2s) with
          | Stream.Cons (p1, _), Stream.Cons (p2, _) ->
              Stream.singleton (p1, p2, p3)
          | _ -> Stream.Empty)
      (* When running out of digits *)
      | _ :: _ -> Stream.Empty
  in
  aux (0, primes, primes, all_digits) p3

let solutions = Stream.flatmap solve primes

let () =
  Stream.iter display solutions;
  Printf.printf "%d solutions\n" (Stream.count solutions)
