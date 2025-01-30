(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Emma Elliott                                 *)
(* Time spent on HW6: 5 hours ish
*)

(* Collaborators and references:
ChatGPT
https://www.cs.tufts.edu/comp/105-2019s/readings/ml.html#unit-testing
https://smlhelp.github.io/book/docs/types/function/
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString "mynull [1] should be false"
    (fn () => mynull [1])
    false

(**** Problem B ****)

(* determines if first char in list is a vowel 
   x :: _ means the first element of the list is 
   being matched *)


fun firstVowel ([ ]) = false
  | firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel _ = false;


val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'bck' should be true"
    (fn () => firstVowel [#"b",#"c",#"k"])
    false

(**** Problem C ****)

fun reverse l = foldl (op :: ) [] l;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

(**** Problem D ****)

exception EmptyListException;

fun minlist (x::xs) = foldl Int.min x xs
  | minlist [] = raise EmptyListException;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

(**** Problem E ****)


exception Mismatch

fun zip ([] , []) = []
  | zip (b :: bs, c :: cs) = (b, c) :: zip (bs, cs)
  | zip _ = raise Mismatch;

(*
The code below I got from chatGPT. Doing these assertions is totally
stumping me to be honest 
*)
  

val () =
  Unit.checkExnWith (fn _ => "Mismatch")   (* Convert exception to string for comparison *)
  "zip ([1], [1,2]) should raise Mismatch"
  (fn () => zip ([1], [1,2]));

fun pairListToString [] = "[]"
  | pairListToString ((x, y) :: xs) =
      "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ")" ^ listHelper xs
and listHelper [] = ""
  | listHelper xs = ", " ^ pairListToString xs;

val () =
  Unit.checkExpectWith pairListToString
  "zip ([1], [1]) should be [(1,1)]"
  (fn () => zip ([1], [1]) )
  [(1, 1)];


(**** Problem F ****)


fun concat ls = foldr op@ [] ls;

fun intListToString [] = "[]"
  | intListToString (x::xs) = 
      "[" ^ Int.toString x ^ listHelper xs ^ "]"
and listHelper [] = ""
  | listHelper (x::xs) = ", " ^ Int.toString x ^ listHelper xs;

val () =
  Unit.checkExpectWith intListToString
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1,2,3,4,5,6]"
  (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6];

val () =
  Unit.checkExpectWith intListToString
  "concat [[]] should be []"
  (fn () => concat [[]])
  [];

(**** Problem G ****)

fun isDigit #"9" = true
  | isDigit #"0" = true
  | isDigit #"1" = true 
  | isDigit #"2" = true
  | isDigit #"3" = true 
  | isDigit #"4" = true
  | isDigit #"5" = true 
  | isDigit #"6" = true
  | isDigit #"7" = true 
  | isDigit #"8" = true
  | isDigit _ = false;



val () =
  Unit.checkExpectWith Bool.toString
  "isDigit #\"a\" should be false"
  (fn () => isDigit #"a")
  false;

val () =
  Unit.checkExpectWith Bool.toString
  "isDigit #\"5\" should be true"
  (fn () => isDigit #"5")
  true;

(**** Problem H ****)

fun isAlpha c =
  (65 <= Char.ord c andalso Char.ord c <= 90) orelse
  (97 <= Char.ord c andalso Char.ord c <= 122);

val () =
  Unit.checkExpectWith Bool.toString
  "isAlpha #\"A\" should be true"
  (fn () => isAlpha #"A")
  true;

val () =
  Unit.checkExpectWith Bool.toString
  "isAlpha #\"a\" should be true"
  (fn () => isAlpha #"a")
  true;

val () =
  Unit.checkExpectWith Bool.toString
  "isAlpha #\"2\" should be false"
  (fn () => isAlpha #"2")
  false; 

(**** Problem I ****)

fun svgCircle (cx, cy, r, fill) = 
  "<circle cx=\"" ^ Int.toString cx ^
  "\" cy=\"" ^ Int.toString cy ^
  "\" r=\"" ^ Int.toString r ^
  "\" fill=\"" ^ fill ^ "\" />";

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";



(**** Problem J ****)

fun partition f lst =
    let
        fun helper [] (yes, no) = (rev yes, rev no)
          | helper (x::xs) (yes, no) =
              helper xs (if f x then (x::yes, no) else (yes, x::no))
    in
        helper lst ([], [])
    end;

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);


(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  
