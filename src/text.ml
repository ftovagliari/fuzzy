let chars_of_string s = s |> String.lowercase_ascii |> String.to_seq |> Array.of_seq;;

let prepare_words text =
  text
  |> String.trim
  |> Str.split (Str.regexp "[ \t\r\n,.;:?!]+")
  |> List.filter (fun w -> String.length w > 3 (*&& Dictionary.words |> List.mem w*))
  |> List.map String.lowercase_ascii
  |> Array.of_list
;;

let prepare_letters str =
  str
  |> String.trim
  |> Str.split (Str.regexp "[ \t\r\n,.;:?!]+")
  |> List.map String.lowercase_ascii
  |> String.concat ""
  |> chars_of_string
;;
