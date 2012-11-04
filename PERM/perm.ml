exception LastPermutation

let swap ind1 ind2 tbl =
  tbl.(ind1) <- (tbl.(ind1) lxor tbl.(ind2));
  tbl.(ind2) <- (tbl.(ind1) lxor tbl.(ind2));
  tbl.(ind1) <- (tbl.(ind1) lxor tbl.(ind2));
;;

let rec reverse ind1 ind2 tbl =
  match (ind2-ind1) with 
    | 0 -> ()
    | 1 -> swap ind1 ind2 tbl
    | _ -> swap ind1 ind2 tbl; 
           reverse (ind1 + 1) (ind2 - 1) tbl
;;


let nextPermutation perm =
  let digits = Array.length perm in
  let rec findK ind =
    match ind with 
      | -1 -> -1
      | ind -> if (perm.(ind) < perm.(ind+1)) then ind else findK (ind-1)
  in   
  let rec findL ind k =
    match ind with 
      | ind when (ind=(k+1)) -> k+1
      | ind -> if (perm.(k) < perm.(ind)) then ind else findL (ind-1) k 
  in   
  let k = findK (digits-2) in
    if (k = -1) then raise LastPermutation;
    let l = findL (digits-1) k in
      swap k l perm;
      reverse (k+1) (digits-1) perm;   
;;

let fact num =
  let rec aux i acc =
    match i with
      | i when (i=num) -> acc*num
      | i -> aux (i+1) (acc*i)
  in
    aux 2 1
;;

let printPerm perm = 
  Array.iter (fun num -> Printf.printf "%d " num) perm;
  Printf.printf "\n"
;;

let solve =
  let n = read_int () in
  let perm = Array.mapi (fun i v -> i + 1) (Array.make n 1) in
  let rec getPermutations () =
    match (try Some (nextPermutation perm) with LastPermutation -> None) with
      | None -> ()
      | Some () -> printPerm perm; getPermutations ();
  in
    Printf.printf "%d\n" (fact n);
    printPerm perm;
    getPermutations ()
;;
