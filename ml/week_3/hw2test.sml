(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test2_3 = get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_1 = get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test3_2 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = 
    [
    {first="Fred", last="Smith", middle="W"}, 
    {first="Fredrick", last="Smith", middle="W"}, 
    {first="Freddie", last="Smith", middle="W"}, 
    {first="F", last="Smith", middle="W"}
    ]

val test4_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 =  remove_card ([(Hearts, Ace),(Hearts, Queen), (Diamonds, Num 2)], (Hearts, Ace), IllegalMove) = [(Hearts,Queen),(Diamonds,Num 2)];
val test7_2 =  remove_card ([(Hearts, Ace),(Hearts, Queen), (Diamonds, Num 2)], (Diamonds, Num 2), IllegalMove) =  [(Hearts,Ace),(Hearts,Queen)];


val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_1 = all_same_color [(Hearts, Ace), (Hearts, Num 2)] = true
val test8_2 = all_same_color [(Diamonds, Ace), (Hearts, Num 2)] = true
val test8_3 = all_same_color [(Spades, Ace), (Hearts, Num 2)] = false
val test8_4 = all_same_color [(Diamonds, Ace), (Hearts, Num 2), (Hearts, Num 3),(Clubs, Num 3)] = false
val test8_5 = all_same_color [(Spades, Ace)] = true
val test8_6 = all_same_color [] = true
val test8_7 = all_same_color [(Spades, Ace), (Hearts, Num 2), (Hearts, Num 3),(Clubs, Num 3)] = false
val test8_8 = all_same_color [(Diamonds, Ace), (Hearts, Num 2), (Clubs, Num 3),(Hearts, Num 3)] = false
val test8_9 = all_same_color [(Diamonds, Ace), (Clubs, Num 2), (Hearts, Num 3),(Hearts, Num 3)] = false
val test8_10 = all_same_color [(Diamonds, Ace), (Hearts, Num 2), (Hearts, Num 3),(Hearts, Num 3)] = true
val test8_11 = all_same_color [(Clubs,Ace),(Spades,Ace),(Diamonds,Ace)] = false

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9_1 = sum_cards [(Clubs, Num 2)] = 2
val test9_2 = sum_cards [(Clubs, Num 2),(Clubs, Num 3), (Clubs, Ace), (Clubs, Queen) ] = 26

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_1 = score ([(Hearts, Num 2),(Clubs, Num 4), (Spades, Ace) ],10) = 21 
val test10_2 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2 
val test10_3 = score ([(Hearts, Num 2)], 15) = 6

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
