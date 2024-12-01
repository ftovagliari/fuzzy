open Printf
open Utils

let chars_of_string s = s |> String.lowercase_ascii |> String.to_seq |> Array.of_seq;;
let debug = true

let matching_positions a b =
  let length_a = Array.length a in
  let length_b = Array.length b in
  let last_a = length_a - 1 in
  let last_b = length_b - 1 in
  let matrix = Array.make_matrix length_a length_b None in
  let paths = Array.make (length_a * length_b) (-1, None) in
  let rec search i j =
    if a.(i) = b.(j) then begin
      matrix.(i).(j) <- Some a.(i);
      let start = i * length_b + j in
      let stop = if i < last_a && j < last_b then (i + 1) * length_b + j + 1 else 0 in
      paths.(start) <- stop, matrix.(i).(j);
      if i < last_a && j < last_b then search (i + 1) (j + 1)
    end
  in
  for i = 0 to last_a do
    for j = 0 to last_b do
      match matrix.(i).(j) with
      | None -> search i j
      | _ -> ()
    done;
  done;
  paths, if debug && !Sys.interactive then Some matrix else None;;

let join edges =
  let paths = ref [] in
  let rec follow i =
    let j, v = edges.(i) in
    edges.(i) <- -1, None;

    (*    edges.(i) <- -1, v (*None*);
          if j = 0 || j = i then [i, v]
          else if j = -1 then []
          else (i, v) :: follow j*)

    match v with
    | Some v' ->
      if j <= 0 || j = i then [i, v']
      else (i, v') :: follow j
    | _ -> []
  in
  edges |> Array.iteri (fun i (e, _) -> if e >= 0 then paths := (follow i) :: !paths);
  !paths;;

(** Removes shortest overlapping paths *)
let reduce lb paths =
  let paths = Array.of_list paths in
  let n_paths = Array.length paths in
  for i = 0 to n_paths - 1 do
    match paths.(i) with
    | [] -> ()
    | ((root_i, _) :: _) as path_i ->
      let bi = root_i / lb in
      for j = i + 1 to n_paths - 1 do
        match paths.(j) with
        | [] -> ()
        | path_j ->
          let ij_overlap = path_j |> List.exists (fun (n, _) -> n / lb = bi) in
          if ij_overlap then
            let k = if List.length path_i <= List.length path_j then i else j in
            paths.(k) <- []
      done
  done;
  paths |> Array.to_list |> List.filter ((<>) []) ;;


(*compare "mysmilarstring" "myawfullysimilarstirng";; (* 74 *)
  compare "mysmilarstring" "mysimilarstring";; (* 97 *)
  compare "similar" "somewhresimlrbetweenthisstring";; (* 45 *)
  compare "foldlstrioght" "List.fold_right";; (* 80 *)
  compare "fildlostri" "LogBuilder.print_timestamp";; (* 81 *)
  compare "fildlistri" "LogBuilder.print_timestamp";; (* 72 *)
  compare "New York Mets vs Atlanta Braves" "Atlanta Braves vs New York Mets";;
  compare "foldlist" "List.fold_left";; (* 92 *)*)

(*
  compare "llength" "Seq.length";;
  compare "llength" "List.length";;
  compare "lilength" "List.length";;
  compare "lifolet2" "List.fold_left2";;
  compare ~simplify:true  "lifole2" "List.fold_left2";;  (*76*)
  compare ~simplify:false "lifole2" "List.fold_left2";;  (*99*)
  compare "lifole2" "List.fold_left";; (*66*)
  compare "follilef2" "List.fold_left";; (*82*)
  compare "lfold2" "List.fold_left2";; (*86*)
*)


