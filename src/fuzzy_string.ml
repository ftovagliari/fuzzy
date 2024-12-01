open Printf
open Utils

let print_matrix m =
  Printf.printf "\n%!";
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length (m.(i)) - 1 do
      if m.(i).(j) <> '\x00' then Printf.printf "%c" m.(i).(j) else Printf.printf "`"
    done;
    Printf.printf "\n%!"
  done;;

let print_path_matrix la lb paths =
  let m = Array.make_matrix la lb '`' in
  paths |> Array.iteri (fun i n ->
      if n >= 0 then begin
        let a = i / lb in
        let b = i mod lb in
        m.(a).(b) <- '*';
        let a = n / lb in
        let b = n mod lb in
        m.(a).(b) <- '*'
      end);
  print_matrix m;;

let matching_positions a b =
  let a = chars_of_string a in
  let b = chars_of_string b in
  let la = Array.length a in
  let lb = Array.length b in
  let m = Array.make_matrix la lb '\x00' in
  let index = ref [] in
  for i = 0 to la - 1 do
    for j = 0 to lb - 1 do
      if Array.unsafe_get a i = Array.unsafe_get b j then begin
        m.(i).(j) <- Array.unsafe_get a i;
        index := (i, j) :: !index;
      end
    done;
  done;
  if !Sys.interactive then print_matrix m;
  la, lb, m, !index;;

let matching_paths la lb m index =
  let mapping = Array.make (la * lb) (-1) in
  if m.(0).(0) <> '\x00' then mapping.(0) <- 0;
  if m.(la - 1).(lb - 1) <> '\x00' then
    mapping.(Array.length mapping - 1) <- Array.length mapping - 1;
  let neighbors (i, j) =
    if i > 0 && j > 0 && m.(i - 1).(j - 1) <> '\x00' then
      let start = (i - 1) * lb + (j - 1) in
      let stop = i * lb + j in
      (*Printf.printf "%d,%d -> %d,%d  --  %d -> %d\n%!" (i-1) (j-1) i j start stop;*)
      mapping.(start) <- stop;
  in
  List.iter neighbors index;
  mapping;;

let join edges =
  let paths = ref [] in
  let rec follow i =
    let j = edges.(i) in
    edges.(i) <- -1;
    if j <= 0 || j = i then [i] else i :: follow j
  in
  edges |> Array.iteri (fun i e -> if e >= 0 then paths := (follow i) :: !paths);
  !paths;;

let remove_shortest_overlapping lb paths =
  let paths = Array.of_list paths in
  let len = Array.length paths in
  for i = 0 to len - 1 do
    match paths.(i) with
    | [] -> ()
    | (root_i :: _) as path_i ->
      let bi = root_i / lb in
      for j = i + 1 to len - 1 do
        match paths.(j) with
        | [] -> ()
        | path_j ->
          let ij_overlap = path_j |> List.exists (fun n -> n / lb = bi) in
          if ij_overlap then begin
            if List.length path_i <= List.length path_j then
              paths.(i) <- []
            else
              paths.(j) <- []
          end
      done
  done;
  paths |> Array.to_list |> List.filter ((<>) []);;

let compare ?(simplify=true) pat str =
  let len_pat, len_str, matrix, index = matching_positions pat str in
  if List.length index > 0 then
    begin
      let paths = matching_paths len_pat len_str matrix index in
      if !Sys.interactive then print_path_matrix len_pat len_str paths;
      let paths = paths |> join |> simplify @|> remove_shortest_overlapping len_str in
      if !Sys.interactive then
        paths |> List.map (fun p -> p |> List.map string_of_int |> String.concat "-") |> String.concat " " |> printf "%s\n%!";
      let lp = float len_pat in
      let ls = float len_str in
      let amount = List.fold_left (fun sum l -> List.length l + sum) 0 paths |> float in
      let number_of_paths = List.length paths in
      let compactness = if number_of_paths > 0 then
          (*let x = float number_of_paths in
            (1.2 *. sin x) /. x*)
          1. /. float number_of_paths
          (*amount /. float number_of_paths*)
        else 0.
      in
      let s_relevance = amount /. ls in
      let p_relevance = amount /. lp in
      let top = lp +. (*lp +.*) 1. +. 2. in
      let score = amount +. compactness +. s_relevance +. p_relevance in
      let score_perc = score /. top in
      if !Sys.interactive then
        Printf.printf "%S %S score:%.2f am:%d cp:%.2f sr:%.2f pr:%.2f lp:%d ls:%d paths:%d\n%!"
          pat str score_perc (int_of_float amount) compactness s_relevance p_relevance len_pat len_str (List.length paths);
      if !Sys.interactive then score_perc
      else if score_perc >= 0.62 then score_perc
      else 0.
    end else 0.
;;

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

