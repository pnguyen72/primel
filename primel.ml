type 'a lazystream = Stream of 'a * 'a lazystream Lazy.t

let rec from n = Stream (n, lazy (from (n + 1)))

let rec take n (Stream (h, next)) =
  if n <= 0 then [] else h :: take (n - 1) (Lazy.force next)
;;

let primes =
  let module M = struct
    include Map.Make (Int)

    let safe_add n p map =
      let rec handle_collision n = if mem n map then handle_collision (n + p) else n in
      add (handle_collision n) p map
    ;;
  end
  in
  let rec primes_from n map =
    match M.find_opt n map with
    | None -> Stream (n, lazy (map |> M.add (n * n) n |> primes_from (n + 1)))
    | Some p -> map |> M.remove n |> M.safe_add (n + p) p |> primes_from (n + 1)
  in
  primes_from 2 M.empty
;;

let rec drop_while p (Stream (h, next) as stream) =
  if p h then drop_while p (Lazy.force next) else stream
;;

let rec take_while p (Stream (h, next)) =
  if p h then h :: take_while p (Lazy.force next) else []
;;

let product l = List.concat_map (fun e' -> l |> List.map (fun e -> e, e'))

let rec to_digits =
  let rec collect acc = function
    | n when n = 0 -> acc
    | n -> collect ((n mod 10) :: acc) (n / 10)
  in
  function
  | n when n < 0 -> to_digits (-n)
  | n when n = 0 -> [ 0 ]
  | n -> collect [] n
;;

let to_number =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux ((acc * 10) + x) xs
  in
  aux 0
;;

(* Checks that a number, represented as list of digits, starts with the specified digits *)
let rec starts_with = function
  | x :: xs ->
    (function
      | y :: ys -> x = y && starts_with xs ys
      | [] -> false)
  | [] -> fun _ -> true
;;

let rec solve (acc_p1, acc_p2, p3, rem_p1, rem_p2, rem_digits) = function
  | [] -> product rem_p1 rem_p2 |> List.map (fun (p1, p2) -> p1, p2, p3)
  | d :: ds ->
    if not (List.mem d rem_digits)
    then []
    else (
      let p3 = p3 @ [ d ] in
      match List.filter (( <> ) d) rem_digits with
      | [] -> []
      | rem_digits ->
        (* If p1's digit matches *)
        let solutions_1 =
          let acc_p1 = acc_p1 @ [ d ] in
          let rem_p1 = rem_p1 |> (acc_p1 |> starts_with |> List.filter) in
          rem_digits
          |> List.concat_map (fun d ->
            let acc_p2 = acc_p2 @ [ d ] in
            let rem_p2 = rem_p2 |> (acc_p2 |> starts_with |> List.filter) in
            solve (acc_p1, acc_p2, p3, rem_p1, rem_p2, rem_digits) ds)
        in
        (* If p2's digit matches *)
        let solutions_2 =
          let acc_p2 = acc_p2 @ [ d ] in
          let rem_p2 = rem_p2 |> (acc_p2 |> starts_with |> List.filter) in
          rem_digits
          |> List.concat_map (fun d ->
            let acc_p1 = acc_p1 @ [ d ] in
            let rem_p1 = rem_p1 |> (acc_p1 |> starts_with |> List.filter) in
            solve (acc_p1, acc_p2, p3, rem_p2, rem_p1, rem_digits) ds)
        in
        solutions_1 @ solutions_2)
;;

let ( << ) = Fun.compose
let print = print_endline << String.concat " " << List.map string_of_int

let () =
  let solutions =
    (* List of 5-digit primes, each is represented as a list of digits *)
    let primes_digits =
      primes
      |> drop_while (fun p -> p < 10000)
      |> take_while (fun p -> p <= 99999)
      |> List.map to_digits
    in
    primes_digits
    |> List.concat_map
         (solve ([], [], [], primes_digits, primes_digits, from 0 |> take 10))
    |> List.filter (fun (p1, p2, p3) ->
      p3 <> p1 && p3 <> p2 && to_number p1 < to_number p2)
  in
  Printf.printf "%d solutions\n" (List.length solutions);
  List.iter
    (fun (p1, p2, p3) ->
       print_newline ();
       print p1;
       print p2;
       print_endline "---------";
       print p3;
       print_newline ())
    solutions
;;
