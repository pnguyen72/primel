let render total covered =
  let ratio = covered /. total in
  let percentage = int_of_float (ratio *. 100.) in
  let completed = int_of_float (ratio *. 50.) in
  let bar = String.make completed '#' ^ String.make (50 - completed) ' ' in
  Printf.printf "\r[%s] (%d%%)%!" bar percentage

let progress_bar (mins, maxes, frontiers) =
  let total =
    maxes |> Array.to_seqi
    |> Seq.fold_left (fun acc (i, max) -> acc + max - mins.(i)) 0
    |> float_of_int
  in
  let update = render total in

  let stop = Atomic.make false in
  let reporter =
    Domain.spawn (fun () ->
        while not (Atomic.get stop) do
          Unix.sleepf 0.2;
          frontiers |> Array.to_seqi
          |> Seq.fold_left (fun acc (i, front) -> acc + front - mins.(i)) 0
          |> float_of_int |> update
        done;
        update total;
        print_newline ())
  in
  fun () ->
    Atomic.set stop true;
    Domain.join reporter
