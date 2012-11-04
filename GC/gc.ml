let contentGC dna = 
    let cnt = ref 0 in  
    String.iter (function 
                   | 'G' -> cnt := !cnt + 1;
                   | 'C' -> cnt := !cnt + 1;
                   | _ -> () ) dna;
    ((float !cnt)/.(float (String.length dna)))

let solve = 
    let rec getInput str =
        match (try Some (input_line stdin) with End_of_file -> None) with
          | Some sub_str -> getInput (String.concat "" [str;sub_str])
          | None -> str
    in
    let rec maxGC lst (max_gc,data_set) = 
        match lst with 
          | [] -> Printf.printf "%s\n%f%%\n" data_set (max_gc*.100.)
          | (Str.Text set_name)::(Str.Delim dna)::tl -> 
              let gc_content = contentGC dna in
              if (gc_content > max_gc) then (maxGC tl (gc_content,set_name))
              else maxGC tl (max_gc,data_set)
          | _ -> failwith "invalid input"
    in
    let all = Str.global_replace (Str.regexp "[>]") "" (getInput "") in
    let lst = Str.full_split (Str.regexp "[A|C|G|T]+") all in
    maxGC lst (0.,"")
;;
