(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["Hola","como","EstasC"] = ["Hola","EstasC"]
val test1_2 = only_capitals ["Hola","cOmo","EstasC"] = ["Hola","EstasC"]


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 [] = ""
val test2_2 = longest_string1 ["A", "bc", "cd"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","B","C"] = "C"
val test2_2 = longest_string2 [] = ""
val test3_3 = longest_string2 ["A", "bc", "cd"] = "cd"


val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 [] = ""
val test4a_2 = longest_string3 ["A", "bc", "cd"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b_1 = longest_string2 [] = ""
val test4b_2 = longest_string4 ["A", "bc", "cd"] = "cd"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME[2,3,4,5,6,7]
val test8_2 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [] = SOME[]

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (Variable("hello")) = 0
val test9a2 = count_wildcards UnitP = 0
val test9a3 = count_wildcards (ConstP(3)) = 0
val test9a4 = count_wildcards (TupleP([Wildcard, UnitP])) = 1
val test9a5 = count_wildcards (TupleP([Wildcard, UnitP, Wildcard])) = 2
val test9a6 = count_wildcards (ConstructorP("helo", Wildcard)) = 1
val test9a7 = count_wildcards (ConstructorP("helo", (TupleP([Wildcard, UnitP, Wildcard])))) = 2

val test9b = count_wild_and_variable_lengths (Variable("abcdefgh")) = 8
val test9b0 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (Variable("hello")) = 5
val test9b2 = count_wild_and_variable_lengths UnitP = 0
val test9b3 = count_wild_and_variable_lengths (ConstP(3)) = 0
val test9b4 = count_wild_and_variable_lengths (TupleP([Wildcard, UnitP])) = 1
val test9b5 = count_wild_and_variable_lengths (TupleP([Wildcard, UnitP, Wildcard, Variable("hello")])) = 7
val test9b6 = count_wild_and_variable_lengths (ConstructorP("helo", Wildcard)) = 1
val test9b7 = count_wild_and_variable_lengths (ConstructorP("helo", (TupleP([Wildcard, UnitP, Wildcard])))) = 2


val test9c = count_some_var ("x", Variable("x")) = 1
val test9c0 = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var ("b", Variable("a")) = 0
val test9c2 = count_some_var ("hello", Variable("hello")) = 1
val test9c3 = count_some_var ("hello", UnitP) = 0
val test9c4 = count_some_var ("x", ConstP(3)) = 0
val test9c5 = count_some_var ("x", TupleP([Wildcard, UnitP])) = 0
val test9c6 = count_some_var ("hello", TupleP([Wildcard, UnitP, Wildcard, Variable("hello")])) = 1
val test9c7 = count_some_var ("hello", ConstructorP("hello", Wildcard)) = 0
val test9c8 = count_some_var ("hello", ConstructorP("hello", (TupleP([Wildcard, UnitP, Variable("hello")])))) = 1

val test10 = check_pat (Variable("x")) = true
val test100 = check_pat (Variable("x")) = true
val test101 = check_pat (UnitP) = true
val test102 = check_pat Wildcard = true
val test103 = check_pat (Variable("hello")) = true
val test105 = check_pat (ConstP(3)) = true
val test106 = check_pat (TupleP([Wildcard, UnitP])) = true
val test107 = check_pat (TupleP([Wildcard, UnitP, Wildcard])) = true
val test108 = check_pat (ConstructorP("helo", Wildcard)) = true
val test109 = check_pat (ConstructorP("helo", (TupleP([Wildcard, UnitP, Wildcard])))) = true
val test110 = check_pat (ConstructorP("helo", (TupleP([Wildcard, Variable("world"), Wildcard])))) = true
val test111 = check_pat (ConstructorP("helo", (TupleP([Variable("hello"), Variable("world"), Wildcard])))) = true
val test112 = check_pat (ConstructorP("helo", (TupleP([Variable("hello"), Variable("hello"), Wildcard])))) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match(Constructor("my_constructor", Const 13), ConstructorP("my_constructor", Variable "my_var")) = SOME[("my_var", Const 13)]
val test12 = first_match Unit [UnitP] = SOME []

