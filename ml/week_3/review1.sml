(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s: string, s_list: string list) =
  let fun aux(s_list: string list, acc: string list, is_in: bool) =
        case s_list of
            [] => if is_in 
                  then SOME (acc)
                  else NONE
          | i::xs => if same_string(s, i)
                     then aux(xs, acc, true)
                     else aux(xs, acc @ [i], is_in)
  in
    aux(s_list, [], false)
  end

fun get_substitutions1(subs: (string list) list, s: string) =
  case subs of
      [] => []
    | sub::xs =>
        case all_except_option(s, sub) of
            NONE => get_substitutions1(xs, s)
          | SOME ss => ss @ get_substitutions1(xs, s)

fun get_substitutions2(subs: (string list) list, s: string) =
  let fun aux(subs: (string list) list, acc: string list) =
        case subs of
            [] => acc
          | sub :: xsub => 
              case all_except_option(s, sub) of
                  NONE => aux(xsub, acc)
                | SOME ss => aux(xsub, acc @ ss)
  in
    aux(subs, [])
  end

fun similar_names(subs: (string list) list, full_name: {first: string, middle:
  string, last: string}) =
  let 
    val {first=x, middle=y, last=z} = full_name
    fun helper(first_names, acc) =
        case first_names of
            [] => acc
          | name::xs => helper(xs, acc @ [{first=name, middle=y, last=z}])
  in
    helper(get_substitutions2(subs, x), [full_name])
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

fun card_color(c: card) =
  case c of
      (Clubs, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red 
    | (Spades, _) => Black

fun card_value(c: card) =
  case c of
      (_, Ace) => 11
    | (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Num va) => va

fun remove_card(cs: card list, c: card, e: exn) =
  let
    fun helper(cs: card list, acc: card list) =
      case cs of 
          [] => raise e 
        | cd :: tcs =>
            if cd = c
            then acc @ tcs
            else helper(tcs, acc @ [cd])
  in
    helper(cs, [])
  end

fun all_same_color(cs: card list) =
  case cs of
      [] => true
    | _ :: [] => true
    | c1 :: c2 :: lcs => 
        if card_color(c1) = card_color(c2)
        then all_same_color(c2 :: lcs)
        else false

fun sum_cards(cs: card list) =
  let
    fun aux(cs: card list, acc: int) =
      case cs of
          [] => acc
        | c::xs => aux(xs, card_value(c) + acc)
  in
    aux(cs, 0)
  end

fun score(cs: card list, goal: int) =
  let
    val p_score = sum_cards(cs)
  in
    (if p_score >= goal then 3 * (p_score - goal) else goal - p_score)
    div (if all_same_color(cs) then 2 else 1)
  end

fun officiate_with_socre(cs: card list, moves: move list, goal: int, score_fun: (card list * int) -> int) =
  let
    fun helper(cs: card list, moves: move list, helds: card list) =
      case moves of
          [] => score_fun(helds, goal)
        | (Discard c) :: lmoves => helper(cs, lmoves, remove_card(helds, c, IllegalMove))
        | Draw :: lmoves =>
            case cs of
                [] => score_fun(helds, goal)
              | c :: xs => if sum_cards(c::helds) > goal
                           then score_fun(c::helds, goal)
                           else helper(xs, lmoves, c::helds)
  in
    helper(cs, moves, [])
  end

fun officiate(cs: card list, moves: move list, goal: int) =
  officiate_with_socre(cs, moves, goal, score)

(* challenge problem here *)
fun ace_num(cs: card list, num: int) =
  case cs of
      [] => num
    | c :: xs =>
        case c of
            (_, Ace) => ace_num(xs, num + 1)
          | (_, _) => ace_num(xs, num)

fun score_challenge(cs: card list, goal: int) =
  let
    val p_score_up = sum_cards(cs)
    val num_ace = ace_num(cs, 0)
    val p_score_low = p_score_up - num_ace * 10
    val s_up = goal - p_score_up
    val s_low = goal - p_score_low

    fun cal_score(p_score: int) =
      (if p_score >= goal then 3 * (p_score - goal) else goal - p_score)
      div (if all_same_color(cs) then 2 else 1)
  in
    case (s_up < 0, s_low < 0) of
        (true, true) => cal_score(p_score_low)
      | (true, false) => cal_score(goal)
      | (false, false) => cal_score(p_score_up)
      | (_, _) => 0 (* impossible *)
  end

fun officiate_challenge(cs: card list, moves: move list, goal: int) =
  officiate_with_socre(cs, moves, goal, score_challenge)
