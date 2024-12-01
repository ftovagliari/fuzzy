#load "str.cma";;
#load "utils.cmo";;
#load "dictionary.cmo";;
#load "text.cmo";;
#load "fuzzy.cmo";;

module FuzzyLetters = Fuzzy.Make(Fuzzy.Letter);;
module FuzzyWords = Fuzzy.Make(Fuzzy.Word);;

let by_letters = [
  "New York Mets vs Atlanta Braves", "Atlanta Braves vs New York Mets";
  "somewhresimlrbetweenthisstring", "similar";
  "mysmilarstring", "myawfullysimilarstirng";
  "mysmilarstring", "mysimilarstring";
  "foldlstrioght", "List.fold_right";
  "lfold2", "List.fold_left2";
  "foldlist", "List.fold_left";
  "fildlostri", "LogBuilder.print_timestamp";
  "lifole2", "List.fold_left";
];;

FuzzyLetters.debug := false;;
by_letters |> List.iter begin fun (a, b) ->
  let score, paths = FuzzyLetters.compare a b in
  Printf.printf "%S %S %2.1f - %s\n%!" a b score
    (paths |> List.map FuzzyLetters.print_path |> String.concat ", ");
end;;


let p3 = {| francesco tovagliari via marzole, 38 suzzara (mn) - 46029 |};;
let p4 = {| tovagliari francesco strada marzole 38 suzzara (mantova) 46029 |};;

let p5 = {|
Terminati questi discorsi, Gesù partì dalla Galilea e andò nel territorio della Giudea, al di là del Giordano.
|};;

let p6 = {|
Quando Gesù ebbe finito questi ragionamenti, partì dalla Galilea e se ne andò sui confini della Giudea oltre il Giordano. |};;

FuzzyLetters.compare ~simplify:true p1 p2;;
FuzzyWords.compare ~simplify:true p5 p6;;

FuzzyWords.compare ~simplify:true p3 p4;;

FuzzyWords.compare ~simplify:true p5 p7;;
FuzzyLetters.compare ~simplify:true p5 p7;;

FuzzyLetters.compare "New York Mets vs Atlanta Braves" "Atlanta Braves vs New York Mets";;


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
