let solve =
    let dna = read_line() in
    let mutated_dna = read_line() in
    let cmp str i c = if (str.[i] == c) then 0 else 1 in
    let hamm = ref 0 in
      String.iteri ( fun i chr -> hamm := !hamm + (cmp mutated_dna i chr) ) dna;
      Printf.printf "%d\n" !hamm
;;
