let solve =
  let dna = read_line () in
  let a = ref 0 in
  let t = ref 0 in
  let g = ref 0 in
  let c = ref 0 in
    String.iter (function 
                   | 'A' -> a := !a + 1
                   | 'T' -> t := !t + 1
                   | 'G' -> g := !g + 1
                   | 'C' -> c := !c + 1
                   | _  -> failwith "not a DNA string\n") dna;
    Printf.printf "%d %d %d %d\n" !a !c !g !t
;;
