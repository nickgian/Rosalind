let solve =
    let input = read_line() in
    let lst = List.map (fun x -> float_of_string x) (Str.split (Str.regexp " ") input) in
    let prob x = ((x ** 2.) +. ((x-.1.)**2.))/.2. in
      List.iter (fun x -> Printf.printf "%f " (prob x)) lst;
      Printf.printf "\n"
;;


[("UUU", 'F');      ("CUU", 'L');      ("AUU", 'I');      ("GUU", 'V');
 ("UUC", 'F');      ("CUC", 'L');      ("AUC", 'I');      ("GUC", 'V');
 ("UUA", 'L');      ("CUA", 'L');      ("AUA", 'I');      ("GUA", 'V');
 ("UUG", 'L');      ("CUG", 'L');      ("AUG", 'M');      ("GUG", 'V');
 ("UCU", 'S');      ("CCU", 'P');      ("ACU", 'T');      ("GCU", 'A');
 ("UCC", 'S');      ("CCC", 'P');      ("ACC", 'T');      ("GCC", 'A');
 ("UCA", 'S');      ("CCA", 'P');      ("ACA", 'T');      ("GCA", 'A');
 ("UCG", 'S');      ("CCG", 'P');      ("ACG", 'T');      ("GCG", 'A');
 ("UAU", 'Y');      ("CAU", 'H');      ("AAU", 'N');      ("GAU", 'D');
 ("UAC", 'Y');      ("CAC", 'H');      ("AAC", 'N');      ("GAC", 'D');
 ("UGU", 'C');      ("CGU", 'R');      ("AGU", 'S');      ("GGU", 'G');
 ("UGC", 'C');      ("CGC", 'R');      ("AGC", 'S');      ("GGC", 'G');
 ("UGG", 'W');      ("CGG", 'R');      ("AGG", 'R');      ("GGG", 'G');
 ("CAA", 'Q');      ("AAA", 'K');      ("GAA", 'E');      ("GAG", 'E');
 ("CGA", 'R');      ("AGA", 'R');      ("GGA", 'G');      ("GAG", 'E');
 ("CAG", 'Q');      ("AAG", 'K');      ("GAG", 'E')]




