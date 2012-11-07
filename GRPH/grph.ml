let solve = 
    let rec getInput str =
        match (try Some (input_line stdin) with End_of_file -> None) with
          | Some sub_str -> getInput (String.concat "" [str;sub_str])
          | None -> str
    in
    let rec makeTuple xs acc = 
        match xs with 
          | [] -> List.rev acc
          | (Str.Text set_name)::(Str.Delim dna)::tl -> makeTuple tl ((set_name,dna) :: acc)
          | _ -> failwith "Invalid input\n"
    in
    let prefix str = String.sub str 0 3 in
    let suffix str = String.sub str ((String.length str) - 3) 3 in
    let all = Str.global_replace (Str.regexp "[>]") "" (getInput "") in
    let dna_lst = makeTuple (Str.full_split (Str.regexp "[A|C|G|T]+") all) [] in
    List.iter (fun (name,dna) -> let suf = suffix dna in 
                                     List.iter (fun (name2,dna2) -> let pre = prefix dna2 in 
                                                                    if ((pre = suf) && (name != name2)) then Printf.printf "%s %s\n" name name2) dna_lst) dna_lst
;;
