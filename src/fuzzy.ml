module type ELEMENT =
sig
  type t
  val to_string : t -> string
  val of_string : string -> t
  val deserialize : string -> t array
end

module Word = struct
  type t = string
  let to_string = Fun.id
  let of_string = Fun.id
  let deserialize = Text.prepare_words
end

module Letter = struct
  type t = char
  let to_string = Stdlib.String.make 1
  let of_string s = s.[0]
  let deserialize = Text.prepare_letters
end

module Make = functor (Elt : ELEMENT) ->
struct

  open Printf
  open Utils
  include Fuzzygen

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

  let print_path_matrix la lb paths =
    let m = Array.make_matrix la lb (Some (Elt.of_string " ")) in
    paths |> Array.iteri (fun i (n, (opt : Elt.t option)) ->
        if n >= 0 then begin
          let s = match opt with Some v -> v | _ -> Elt.of_string "*" in
          let a = i / lb in
          let b = i mod lb in
          m.(a).(b) <- Some s;
          (*          let a = n / lb in
                      let b = n mod lb in
                      m.(a).(b) <- Some (Elt.of_string "$")*)
        end);
    print_matrix m;;

  let compare ?(simplify=true) pat str =
    let pat = Elt.deserialize pat in
    let str = Elt.deserialize str in
    let len_pat, len_str = Array.length pat, Array.length str in
    let paths, debug_matrix = matching_positions pat str in
    if !Sys.interactive then
      paths |> Array.to_list |> List.iteri begin fun i (n, v) ->
        Printf.printf "%3d -> %3d %s\n%!" i n (
          match v with
          | Some v' -> Elt.to_string v'
          | _ -> ""
        );
      end;
    debug_matrix |> Option.iter begin fun matrix ->
      print_matrix matrix;
      print_path_matrix len_pat len_str paths
    end;
    let paths = paths |> join |> simplify @|> reduce len_str in
    if !Sys.interactive then
      paths |> List.rev |> List.map begin fun p ->
        p |> List.map (fun (_, v) -> Elt.to_string v) |> List.filter ((<>) "") |> String.concat "-"
      end |> String.concat ", " |> printf "%s\n%!";
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
    if !Sys.interactive then
      Printf.printf "score:%.2f am:%d cp:%.2f sr:%.2f pr:%.2f lp:%d ls:%d paths:%d\n%!"
        score_perc (int_of_float amount) compactness s_relevance p_relevance len_pat len_str (List.length paths);
    if !Sys.interactive then score_perc
    else if score_perc >= 0.62 then score_perc
    else 0.
  ;;

end
