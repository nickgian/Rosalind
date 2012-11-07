let solve =
  let (m,n) = 
    match List.map (fun x -> float_of_string x) (Str.split (Str.regexp " ") (read_line())) with
      | m::n::[] -> (m,n)
      | _ -> failwith "Invalid input\n"
  in
  let lst = List.map (fun x -> float_of_string x) (Str.split (Str.regexp " ") (read_line())) in
  let eVal x = ((((x ** 2.)+.((1.-.x)**2.))/.2.)**m)*.(n-.m+.1.) in
    List.iter (fun x -> Printf.printf "%f " (eVal x)) lst;
    Printf.printf "\n"
;;
