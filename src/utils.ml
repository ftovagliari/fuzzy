let [@inline] ( @|> ) g f = if g then f else Fun.id

let distance s t =
  let m = String.length s in
  let n = String.length t in
  let d = Array.make_matrix (m + 1) (n + 1) 0 in
  for i = 1 to m do d.(i).(0) <- i done;
  for j = 1 to n do d.(0).(j) <- j done;
  for j = 1 to n do
    for i = 1 to m do
      d.(i).(j) <-
        if s.[i - 1] = t.[j - 1] then d.(i - 1).(j - 1)
        else
          let deletion = d.(i - 1).(j) + 1 in
          let insertion = d.(i).(j - 1) + 1 in
          let substitution = d.(i - 1).(j - 1) + 1 in
          min (min deletion insertion) substitution
    done
  done;
  d.(m).(n);;

let lcs =
  let longest xs ys = if List.length xs > List.length ys then xs else ys in
  let list_of_string str =
    let result = ref [] in
    String.iter (fun x -> result := x :: !result)
      str;
    List.rev !result
  in
  fun xs' ys' ->
    let xs' = list_of_string xs' in
    let ys' = list_of_string ys' in
    let xs = Array.of_list xs'
    and ys = Array.of_list ys' in
    let n = Array.length xs
    and m = Array.length ys in
    let a = Array.make_matrix (n+1) (m+1) [] in
    for i = n-1 downto 0 do
      for j = m-1 downto 0 do
        a.(i).(j) <- if xs.(i) = ys.(j) then
            xs.(i) :: a.(i+1).(j+1)
          else
            longest a.(i).(j+1) a.(i+1).(j)
      done
    done;
    a.(0).(0);;

module File = struct

  let read_all_lines filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines
  ;;
end
