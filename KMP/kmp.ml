let iter f n =
  let rec aux i = 
    match i with
      | i when i = n -> ()
      | i -> f i; aux (i+1);
  in
    aux 1
;;

let solve =
  let dna = read_line () in
  let dna_len = String.length dna in
  let b = Array.make dna_len 0 in
  let rec check chr i =
    match b.(i) with 
      | 0 -> if (chr = dna.[0]) then 1 else 0  
      | _ -> if (dna.[b.(i)] = chr) then b.(i)+1 else check chr (b.(i)-1)
  in
    Printf.printf "0 ";
    iter (fun i -> b.(i) <- (check dna.[i] (i-1)); Printf.printf "%d " b.(i)) dna_len;
;;
