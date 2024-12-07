module type ELEMENT =
sig
  type t
  val to_string : t -> string
  val separator : string
  val of_string : string -> t
  val deserialize : string -> t array
end

module Word = struct
  type t = string
  let to_string = Fun.id
  let of_string = Fun.id
  let deserialize = Text.prepare_words
  let separator = "-"
end

module Letter = struct
  type t = char
  let to_string = Stdlib.String.make 1
  let of_string s = s.[0]
  let deserialize = Text.prepare_letters
  let separator = ""
end

module Make = functor (Elt : ELEMENT) ->
struct

  open Printf
  open Utils

  type path = Elt.t list

  let print_path (path : path) = path |> List.map Elt.to_string |> String.concat Elt.separator

  let print_matrix m =
    Printf.printf "\n%!";
    for i = 0 to Array.length m - 1 do
      for j = 0 to Array.length (m.(i)) - 1 do
        match m.(i).(j) with
        | Some value ->printf "%s," (Elt.to_string value)
        | _ -> printf " ,"
      done;
      Printf.printf "\n%!"
    done;;

  let debug = ref true

  let find_common_paths a b =
    let length_a = Array.length a in
    let length_b = Array.length b in
    let last_a = length_a - 1 in
    let last_b = length_b - 1 in
    let matrix = Array.make_matrix length_a length_b None in
    let path = ref [] in
    let paths = ref [] in
    let end_path () =
      paths := !path :: !paths;
      path := [];
    in
    let rec search i j has_prev =
      if a.(i) <> b.(j) then begin
        if has_prev then end_path()
      end else begin
        matrix.(i).(j) <- Some a.(i);
        path := ((i, j), ref a.(i)) :: !path;
        if i >= last_a || j >= last_b then end_path()
        else search (i + 1) (j + 1) true
      end
    in
    for i = 0 to last_a do
      for j = 0 to last_b do
        match matrix.(i).(j) with
        | None -> search i j false
        | _ -> () (* Skip the matrix cells already processed *)
      done;
    done;
    let paths =
      !paths
      |> List.rev
      |> List.map begin fun path ->
        path |> List.rev |> List.map (fun (pos, { contents = v }) -> pos, v)
      end
    in
    paths, if !debug && !Sys.interactive then Some matrix else None;;

  (** Removes shortest overlapping paths *)
  let reduce paths =
    let paths = Array.of_list paths in
    let n_paths = Array.length paths in
    for i = 0 to n_paths - 1 do
      match paths.(i) with
      | [] -> ()
      | (((xi, yi), _) :: _) as path_i ->
        let len_i = List.length path_i in
        for j = 0 to n_paths - 1 do
          if i <> j then
            match paths.(j) with
            | [] -> ()
            | path_j ->
              let ij_overlap = path_j |> List.exists (fun ((x, y), _) -> xi = x || yi = y) in
              if ij_overlap then
                let k = if len_i <= List.length path_j then i else j in
                paths.(k) <- []
        done
    done;
    paths |> Array.to_list |> List.filter ((<>) [])
  ;;

  let compare ?(simplify=true) pat str =
    let pat = Elt.deserialize pat in
    let str = Elt.deserialize str in
    let len_pat, len_str = Array.length pat, Array.length str in
    let paths, debug_matrix = find_common_paths pat str in
    let paths = paths |> simplify @|> reduce in
    let paths = paths |> List.map (fun p -> p |> List.map (fun (_, v) -> v)) in
    Option.iter print_matrix debug_matrix;
    if !debug && !Sys.interactive then
      paths
      |> List.map (fun p -> p |> List.map Elt.to_string |> String.concat "-")
      |> String.concat ", " |> printf "%s\n%!";
    let lp = float len_pat in
    let ls = float len_str in
    let amount = List.fold_left (fun sum l -> List.length l + sum) 0 paths |> float in
    let number_of_paths = List.length paths in
    let compactness = if number_of_paths > 0 then 1. /. float number_of_paths else 0. in
    let s_relevance = amount /. ls in
    let p_relevance = amount /. lp in
    let top = lp +. (*lp +.*) 1. +. 2. in
    let score = amount +. compactness +. s_relevance +. p_relevance in
    let score_perc = score /. top in
    if !debug && !Sys.interactive then
      Printf.printf "score:%.2f am:%d cp:%.2f sr:%.2f pr:%.2f lp:%d ls:%d paths:%d\n%!"
        score_perc (int_of_float amount) compactness s_relevance p_relevance len_pat len_str (List.length paths);
    let score =
      if !Sys.interactive then score_perc
      else if score_perc >= 0.62 then score_perc
      else 0.
    in
    score, (paths : path list)
  ;;

end
