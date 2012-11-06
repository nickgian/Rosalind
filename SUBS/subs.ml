let search str pattern =
  let pattern_len = String.length pattern in
  let str_len = String.length str in
  let rec aux str pattern i = 
    match (try (Some (Str.search_forward (Str.regexp pattern) str i)) with Not_found -> None) with
      | Some next -> Printf.printf "%d " (next+1); if (str_len > (next+pattern_len)) then  aux str pattern (next+1) 
      | None -> ()
  in
    aux str pattern 0
;;

let solve =
  let dna = read_line() in
  let pattern = read_line() in
    search dna pattern;
    Printf.printf "\n"
;;
