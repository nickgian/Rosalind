let solve =
  let dna = read_line () in
  let rna = String.map (function | 'T' -> 'U'
                                 | other -> other) dna in
  Printf.printf "%s\n" rna
;;
