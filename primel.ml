(* ------ Logic helpers ------ *)

let ( << ) = Fun.compose
let ( >> ) f g x = g (f x)
let rec to_digits = function 0 -> [] | n -> to_digits (n / 10) @ [ n mod 10 ]
let from_digits = List.fold_left (fun acc d -> (acc * 10) + d) 0

let rec last = function
  | [] -> failwith "Empty list"
  | [ x ] -> x
  | _ :: xs -> last xs

let rec matches idx v = function
  | x :: _ when idx = 0 -> x = v
  | _ :: xs -> matches (idx - 1) v xs
  | [] -> false

(* Because there are only 10 digits, using List is actually faster than Set *)
let all_digits = List.init 10 Fun.id
let remove v = List.filter (( <> ) v)

(* ------ Progress bar & parallelism ------ *)

let progress_bar total covered =
  let ratio = covered /. total in
  let percentage = int_of_float (ratio *. 100.) in
  let completed = int_of_float (ratio *. 50.) in
  let bar = String.make completed '#' ^ String.make (50 - completed) ' ' in
  Printf.printf "\r[%s] (%d%%)%!" bar percentage

let parallel_progress_bar (mins, maxes, frontiers) =
  let total =
    maxes |> Array.to_seqi
    |> Seq.fold_left (fun acc (i, max) -> acc + max - mins.(i)) 0
    |> float_of_int
  in
  let progress = progress_bar total in

  let stop = Atomic.make false in
  let reporter =
    Domain.spawn (fun () ->
        while not (Atomic.get stop) do
          Unix.sleepf 0.2;
          frontiers |> Array.to_seqi
          |> Seq.fold_left (fun acc (i, front) -> acc + front - mins.(i)) 0
          |> float_of_int |> progress
        done;
        progress total;
        print_newline ())
  in
  fun () ->
    Atomic.set stop true;
    Domain.join reporter

let to_chunks n =
  let rec split chunk n list =
    match (n, list) with
    | 0, _ | _, [] -> (List.rev chunk, list)
    | _, x :: xs -> split (x :: chunk) (n - 1) xs
  in
  let rec collect chunks = function
    | [] -> List.rev chunks
    | list ->
        let chunk, rest = split [] n list in
        collect (chunk :: chunks) rest
  in
  if n < 1 then failwith "Chunk size must be positive" else collect []

let parallelize chunk_count domain f =
  let chunk_size = (List.length domain + chunk_count - 1) / chunk_count in
  let chunks = to_chunks chunk_size domain in

  let chunk_mins = chunks |> List.map List.hd |> Array.of_list in
  let chunk_maxes = chunks |> List.map last |> Array.of_list in
  let frontiers = Array.copy chunk_mins in
  let callback i v = frontiers.(i) <- v in

  let stop_progress =
    parallel_progress_bar (chunk_mins, chunk_maxes, frontiers)
  in
  let result =
    chunks
    |> List.mapi (fun i chunk -> Domain.spawn (fun () -> f (callback i) chunk))
    |> List.map Domain.join
  in
  stop_progress ();
  result

(* ------ Main ------ *)

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
  let candidates = primes |> Stream.to_stream |> Stream.map to_digits in
  aux (0, candidates, candidates, all_digits) p3_digits

let () =
  let out_file = "solutions.txt" in
  let oc = open_out out_file in
  let prime_list = Stream.to_list primes in
  print_endline "Calculating...";
  let result =
    parallelize 12 prime_list (fun on_progress ->
        Stream.to_stream
        >> Stream.flatmap (solve prime_list)
        >> Stream.map (fun (p1, p2, p3) ->
            Printf.fprintf oc "(%d, %d, %d)\n" p1 p2 p3;
            on_progress p3)
        >> Stream.count)
    |> List.fold_left ( + ) 0
  in
  Printf.printf "Solutions: %d\n" result;
  Printf.printf "Output written to %s\n" out_file
