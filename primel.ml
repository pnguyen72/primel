(* ------ Helpers ------ *)

let ( << ) = Fun.compose
let rec to_digits = function 0 -> [] | n -> to_digits (n / 10) @ [ n mod 10 ]
let from_digits = List.fold_left (fun acc d -> (acc * 10) + d) 0

let rec matches idx v = function
  | x :: _ when idx = 0 -> x = v
  | _ :: xs -> matches (idx - 1) v xs
  | [] -> false

(* Because there are only 10 digits, using List is actually faster than Set *)
let all_digits = List.init 10 Fun.id
let remove v = List.filter (( <> ) v)

let progress_bar min max =
  let fmin = float_of_int min and fmax = float_of_int max in
  let last_percentage = ref (-1) in
  let handle item =
    let ratio = (float_of_int item -. fmin) /. (fmax -. fmin) in
    let percentage = int_of_float (ratio *. 100.) in
    if percentage <> !last_percentage then (
      let bar ratio =
        let completed = int_of_float (ratio *. 50.) in
        let remaining = 50 - completed in
        String.make completed '#' ^ String.make remaining ' '
      in
      Printf.printf "\r[%s] (%d%%)%!" (bar ratio) percentage;
      last_percentage := percentage)
  in
  print_endline "Calculating...";
  (* To show the bar immediately *)
  handle min;
  let rec aux = function
    | Stream.Cons (item, next) ->
        handle item;
        aux (Lazy.force next)
    | _ ->
        (* To make the bar complete with 100% *)
        handle max;
        print_newline ()
  in
  aux

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

let primes =
  primes_generator
  |> Stream.drop_while (fun p -> p < 10000)
  |> Stream.take_while (fun p -> p <= 99999)

let primes_digits = Stream.map to_digits primes

(* Given p3, find all solutions (p1, p2, p3) *)
let solve p3 =
  let p3_digits = to_digits p3 in
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
        let narrow = Stream.filter << matches n in
        function
        | d :: ds when List.mem d digits ->
            let digits = remove d digits in
            (* For each digit of p3, either p1 or p2 matches it.
            hits = remaining primes for the one that matches
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
            (* Generate solutions for both cases, then concatenate *)
            Stream.cat
              (branch (fun x y -> (x, y)) p1s p2s)
              (branch (fun x y -> (y, x)) p2s p1s)
        | [] -> (
            match (p1s, p2s) with
            | Stream.Cons (p1_digits, _), Stream.Cons (p2_digits, _) ->
                let p1 = from_digits p1_digits and p2 = from_digits p2_digits in
                Stream.singleton (p1, p2, p3)
            | _ -> Stream.Empty)
        (* When running out of digits *)
        | _ :: _ -> Stream.Empty
  in
  aux (0, primes_digits, primes_digits, all_digits) p3_digits

let solutions = Stream.flatmap solve primes

let () =
  let out_file_name = "solutions.txt" in
  let oc = open_out out_file_name in
  solutions
  |> Stream.map (fun (p1, p2, p3) ->
      Printf.fprintf oc "%d, %d, %d\n" p1 p2 p3;
      p3)
  |> progress_bar
       (primes |> Stream.first ~default:10000)
       (primes |> Stream.last ~default:99999);
  close_out oc;
  Printf.printf "Solutions: %d\n" (Stream.count solutions);
  Printf.printf "Full results written to %s\n" out_file_name
