let solve =
  let dna = read_line () in
  let compl dna = String.map (function 
                                | 'A' -> 'T'
                                | 'T' -> 'A'
                                | 'G' -> 'C'
                                | 'C' -> 'G'
                                | _ -> failwith "not a DNA string\n") dna;
  in 
  let reverse str =
    let len = String.length str in
    let res = String.create len in
      for i = 0 to (len-1) do
        res.[i] <- str.[len - i - 1]
      done;
      (res)
  in
    Printf.printf "%s\n" (reverse (compl dna))
;;
