let reverse str =
  let len = String.length str in
  let res = String.create len in
    for i = 0 to (len-1) do
      res.[i] <- str.[len - i - 1]
    done;
    (res)
;;
let comple = function 
  | 'A' -> 'T'
  | 'T' -> 'A'
  | 'G' -> 'C'
  | 'C' -> 'G'
  | _ -> failwith "not a DNA string\n"
;;

let complement dna = String.map comple dna
;;

let isRevCompl dna = (dna = reverse ( complement dna));;

let findRevCompl dna compl_dna n =
  let len = String.length dna in
  let rec iter i acc =
    match i with
      | i when i > (len-n) -> acc
      | i -> if ((String.sub dna i n) = (reverse (String.sub compl_dna i n))) 
        then iter (i+1) ((i, n) :: acc)
        else iter (i+1) acc
  in
    iter 0
;;

let rec expandAll dna tuples acc =
  let dnaLen = String.length dna in
  let rec expand (n,len) i acc =
    match i with 
      | i when ((n-i) < 0) || ((n+len+i-1) >= dnaLen) || (i > 4) -> acc
      | i -> if (dna.[n-i] == (comple (dna.[n+len+i-1])))
        then expand (n,len) (i+1) ((n-i,len+2*i) :: acc)
        else acc
  in
    match tuples with
      | [] -> acc
      | x :: t -> expandAll dna t (expand x 1 acc) 
;;

let solve =
  let dna = read_line () in
  let dna_compl = complement dna in
  let rev_compl4 = findRevCompl dna dna_compl 4 [] in
  let result  = expandAll dna rev_compl4 rev_compl4 in
    List.iter (fun (s,n) -> Printf.printf "%d %d\n" (s+1) n) (List.sort (fun (x,_) (y,_) -> compare x y) result)
;;
