let iter f n =
    let rec aux i = 
      match i with
        | i when i = n -> ()
        | i -> f i; aux (i+1);
    in
    aux 0
;;


let solve =
    let rec readDnaStrings acc = 
        match (try Some (input_line stdin) with End_of_file -> None) with 
          | Some dna -> readDnaStrings (dna::acc)
          | None     -> acc
    in
    let dna_list = readDnaStrings [] in    
    let dna_len = String.length (List.hd dna_list) in
    let a = Array.make dna_len 0 in
    let t = Array.make dna_len 0 in
    let g = Array.make dna_len 0 in
    let c = Array.make dna_len 0 in
    let profile i chr =
        match chr with
            | 'A' -> a.(i) <- a.(i) + 1;
            | 'T' -> t.(i) <- t.(i) + 1;
            | 'C' -> c.(i) <- c.(i) + 1;
            | 'G' -> g.(i) <- g.(i) + 1;
            | _ -> failwith "not a dna string\n"
    in
    List.iter (fun dna -> String.iteri (fun i c -> profile i c) dna) dna_list;
    let compare i = 
        match (a.(i),t.(i),g.(i),c.(i)) with
          | (a,t,g,c) when ((a>=t)&&(a>=g)&&(a>=c)) -> 'A'
          | (a,t,g,c) when ((t>=a)&&(t>=g)&&(t>=c)) -> 'T'
          | (a,t,g,c) when ((g>=t)&&(g>=a)&&(g>=c)) -> 'G'
          | (a,t,g,c) when ((c>=t)&&(c>=g)&&(c>=a)) -> 'C'
          | _ -> failwith "internal error\n"
    in     
    let ppArray arr char = Printf.printf "%c: " char; Array.iter (fun n -> Printf.printf "%d " n) arr; Printf.printf "\n" in
    iter (fun i -> Printf.printf "%c" (compare i))  dna_len;
    Printf.printf "\n";
    ppArray a 'A';
    ppArray c 'C';
    ppArray g 'G';
    ppArray t 'T'
;;
