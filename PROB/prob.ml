let solve =
    let input = read_line() in
    let lst = List.map (fun x -> float_of_string x) (Str.split (Str.regexp " ") input) in
    let prob x = ((x ** 2.) +. ((x-.1.)**2.))/.2. in
      List.iter (fun x -> Printf.printf "%f " (prob x)) lst;
      Printf.printf "\n"
;;
