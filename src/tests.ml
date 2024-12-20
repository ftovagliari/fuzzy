#load "str.cma";;
#load "utils.cmo";;
#load "dictionary.cmo";;
#load "text.cmo";;
#load "fuzzy.cmo";;

module FuzzyLetters = Fuzzy.Make(Fuzzy.Letter);;
module FuzzyWords = Fuzzy.Make(Fuzzy.Word);;

FuzzyLetters.compare `Brute "new_york_mets-vs-atlanta_braves" "atlanta_braves-vs-new_york_mets";;
FuzzyLetters.compare `Brute"asxybs" "bsxyas";;
FuzzyLetters.compare `Greedy"asxybs" "bsxyas";;
FuzzyLetters.compare `Greedy2"asxybs" "bsxyas";;
FuzzyLetters.compare ~simplify:false `Brute"casxybas" "basxycas";;
FuzzyLetters.compare ~simplify:true `Greedy"casxybas" "basxycas";;
FuzzyLetters.compare ~simplify:true `Greedy2"casxybas" "basxycas";;

FuzzyLetters.compare `Brute "folderfold" "foldfolder";;
FuzzyLetters.compare `Greedy "folderfold" "foldfolder";;
FuzzyLetters.compare `Greedy2 "folderfold" "foldfolder";;


FuzzyLetters.compare `Brute "foldfolder" "folderfold";;
FuzzyLetters.compare `Greedy "foldfolder" "folderfold";;
FuzzyLetters.compare `Greedy2 "foldfolder" "folderfold";;

FuzzyLetters.compare `Brute "foldlisleftlist" "List.fold_left2";;
FuzzyLetters.compare `Greedy "foldlisleftlist" "List.fold_left2";;
FuzzyLetters.compare `Greedy2"foldlisleftlist" "List.fold_left2";;

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
  "bcdbc", "bcd"; (* FIX reduce *)
  "bcd", "bcdbc";
];;
FuzzyLetters.debug := true;;
by_letters |> List.iter begin fun (a, b) ->
  let score, paths = FuzzyLetters.compare a b in
  Printf.printf "%S %S %.2f - %s\n%!" a b score
    (paths |> List.map FuzzyLetters.print_path |> String.concat ", ");
end;;


let p3 = {| francesco tovagliari via marzole, 38 suzzara (mn) - 46029 |};;
let p4 = {| tovagliari francesco strada marzole 38 suzzara (mantova) 46029 |};;

let p5 = {|
Terminati questi discorsi, Gesu parti dalla Galilea e ando nel territorio della Giudea, al di la del Giordano.
|};;

let p6 = {|
Quando Gesu ebbe finito questi ragionamenti, parti dalla Galilea e se ne ando sui confini della Giudea oltre il Giordano. |};;

FuzzyLetters.compare ~simplify:true p1 p2;;
FuzzyWords.compare ~simplify:true p5 p6;;

FuzzyLetters.compare ~simplify:false `Brute p3 p4;;
FuzzyLetters.compare `Greedy p3 p4;;
FuzzyLetters.compare `Greedy2 p3 p4;;

FuzzyLetters.compare ~simplify:true `Brute p5 p6;;
FuzzyLetters.compare `Greedy p5 p6;;
FuzzyLetters.compare `Greedy2 p5 p6;;


FuzzyLetters.compare `Brute"New York Mets vs Atlanta Braves" "Atlanta Braves vs New York Mets";;
FuzzyLetters.compare `Greedy"New York Mets vs Atlanta Braves" "Atlanta Braves vs New York Mets";;
FuzzyWords.compare `Brute "New_ York Mets _vs_ Atlanta Braves" "Atlanta Braves _vs_ New_ York Mets";;


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
