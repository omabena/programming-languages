(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, strList) =
  case strList of
      [] => NONE
    | first :: rest => if same_string(str, first)
                       then SOME rest
                       else case all_except_option(str, rest) of
                                NONE => NONE
                              | SOME lst => SOME (first :: lst)

fun get_substitutions1 (substitutions, s) =
  case substitutions of
      [] => []
    | first :: rest => case all_except_option(s, first) of
                           SOME lst => lst @ get_substitutions1(rest, s)
                         | NONE => get_substitutions1(rest, s)

fun get_substitutions2 (substitutions, s) =
  let fun aux(subs, acc) =
        case subs of
            [] => acc
          | first :: rest => case all_except_option(s, first) of
                                 SOME lst => aux(rest, acc @ lst)
                               | NONE => aux(rest, acc)
  in aux(substitutions, [])
  end

fun similar_names (substitutions, {first=firstName, middle=middleName, last=lastName}) =
  let val nameList = get_substitutions2(substitutions, firstName)
      fun aux(lst, acc) =
        case lst of
            [] => acc
          | first :: rest =>
            aux(rest, acc @ [{first=first, middle=middleName, last=lastName}])
  in aux(nameList, [{first=firstName, middle=middleName, last=lastName}])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (s : suit, _) =
  case s of
      Spades => Black
    | Clubs => Black
    | Hearts => Red
    | Diamonds => Red

fun card_value (_, r : rank) =
  case r of
      Num i => i
    | Jack => 10
    | Queen => 10
    | King => 10
    | Ace => 11

fun remove_card (cs : card list, c : card, e) =
  case cs of
      [] => raise e
    | first :: rest => if first = c
                       then rest
                       else first :: remove_card(rest, c, e)

fun all_same_color (cs : card list) =
  case cs of
      [] => true
    | c :: [] => true
    | c1 :: c2 :: rest =>
      card_color(c1) = card_color(c2) andalso all_same_color(c2 :: rest)

fun sum_cards (cs: card list) =
  let fun sum (lst, acc) =
        case lst of
            [] => acc
          | first :: rest => sum(rest, card_value(first) + acc)
  in sum(cs, 0)
  end

fun score (cs : card list, goal : int) =
  let val sum = sum_cards cs
      val preliminary = if sum > goal
                        then 3 * (sum - goal)
                        else goal - sum
  in if all_same_color cs
     then preliminary div 2
     else preliminary
  end

fun officiate (cardList, moveList, goal) =
  let fun run (held : card list, cs : card list, ms : move list, sum : int) =
        case (cs, ms) of
            (_, []) => score(held, goal)
          | (_, (Discard c) :: ms') =>
            run (remove_card(held, c, IllegalMove), cs, ms', sum - card_value(c))
          | ([], Draw :: ms') => score(held, goal)
          | (c :: cs', Draw :: ms') =>
            if sum + card_value(c) > goal
            then score(c :: held, goal)
            else run (c :: held, cs', ms', sum + card_value(c))
  in run([], cardList, moveList, 0)
  end

(* problem 3(a) *)
fun score_challenge (cardList, goal) =
  let fun count_ace (cs : card list) =
        let fun aux(lst, acc) =
              case lst of
                  [] => acc
                | (_, Ace) :: rest => aux(rest, acc + 1)
                | _ :: rest => aux(rest, acc)
        in aux(cs, 0)
        end

      fun best_sum (cs : card list) =
        let val highest = sum_cards(cs)
            val lowest = highest - count_ace(cs) * 10
            fun loop (cur, min) =
              if cur > highest then min
              else
                  if cur > goal
                  then loop(cur + 10, Int.min(min, 3 * (cur - goal)))
                  else loop(cur + 10, Int.min(min, goal - cur))
        in if lowest > goal then loop(lowest + 10, 3 * (lowest - goal))
           else loop(lowest + 10, goal - lowest)
        end
      val sum = best_sum(cardList)
  in if all_same_color cardList
     then sum div 2
     else sum
  end

fun officiate_challenge (cardList, moveList, goal) =
  let fun run (held : card list, cs : card list, ms : move list, sum : int) =
        case (cs, ms) of
            (_, []) => score_challenge(held, goal)
          | (_, (Discard (s, Ace) :: ms')) =>
            run (remove_card(held, (s, Ace), IllegalMove), cs, ms', sum - 1)
          | (_, (Discard c) :: ms') =>
            run (remove_card(held, c, IllegalMove), cs, ms', sum - card_value(c))
          | ([], Draw :: ms') => score_challenge(held, goal)
          | ((s, Ace) :: cs', Draw :: ms') =>
            if sum + 1 > goal
            then score_challenge((s, Ace) :: held, goal)
            else run((s, Ace) :: held, cs', ms', sum + 1)
          | (c :: cs', Draw :: ms') =>
            if sum + card_value(c) > goal
            then score_challenge(c :: held, goal)
            else run (c :: held, cs', ms', sum + card_value(c))
  in run([], cardList, moveList, 0)
  end

(* problem 3(b) *)
fun careful_player (cardList, goal) =
  let fun find (held, top, sum) =
        case held of
            [] => NONE
          | first :: rest =>
            if score(top::remove_card(held, first, IllegalMove), goal)=0
            then SOME first
            else find(rest, top, sum)

      fun loop (held, cs, sum, acc) =
        if score(held, goal)=0 then acc
        else case cs of
                 [] => if goal > sum + 10 then acc @ [Draw] else acc
               | c :: cs' =>
                 if goal > sum + 10
                 then loop(c :: held, cs', sum + card_value(c), acc @ [Draw])
                 else case find (held, c, sum) of
                          NONE => acc
                        | SOME x => acc @ [Discard(x), Draw]
  in loop([], cardList, 0, [])
  end
