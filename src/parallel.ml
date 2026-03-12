open ProgressBar

let parallelize chunk_count f domain =
  let chunk_size = (List.length domain + chunk_count - 1) / chunk_count in
  let chunks = List.to_chunks chunk_size domain in

  let chunk_mins = chunks |> List.map List.hd |> Array.of_list in
  let chunk_maxes = chunks |> List.map List.last |> Array.of_list in
  let frontiers = Array.copy chunk_mins in

  let stop_progress = progress_bar (chunk_mins, chunk_maxes, frontiers) in
  let result =
    chunks
    |> List.mapi (fun i chunk ->
        Domain.spawn (fun () -> f (Array.set frontiers i) chunk))
    |> List.map Domain.join
  in
  stop_progress ();
  result
