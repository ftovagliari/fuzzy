#load "str.cma";;
#load "utils.cmo";;
#load "dictionary.cmo";;
#load "text.cmo";;
#load "fuzzygen.cmo";;
#load "fuzzy.cmo";;

module FuzzyLetters = Fuzzy.Make(Fuzzy.Letter);;
module FuzzyWords = Fuzzy.Make(Fuzzy.Word);;

let p1 = {| b c d e f g h |};;
let p2 = {| k b c k e f b c d z h |};;

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

FuzzyWords.compare ~simplify:false p5 p6;;
FuzzyWords.compare ~simplify:true p5 p7;;
FuzzyLetters.compare ~simplify:true p5 p7;;

