(* Helper function provided by DAN*)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1 (a) *)
fun all_except_option(s: string, lst: string list) =
  let fun aux lst =
	case lst of
	    [] => []
	  | x::xs' => if same_string(x,s) then xs' else x::aux(xs')
      val except_list = aux(lst)											
  in
      if aux(lst) = lst then NONE else SOME except_list
  end
      
(* 1 (b) *)
fun get_substitutions1 (list: string list list, s: string) =
  case list of
      [] => []
    | x::xs' => let val filteredList = all_except_option(s, x) in
		    case filteredList of
			NONE => get_substitutions1(xs',s)
		      | SOME l => l @ get_substitutions1(xs',s)
		end

(* 1 (c) *)
fun get_substitutions2(lst: string list list, s: string) =
  let fun aux(lst, acc) =
	case lst of
	    [] => acc
	  | x::xs'  => let val filteredList = all_except_option(s, x) in
			   case filteredList of
			       NONE => aux(xs', acc)
			     | SOME i => aux(xs', acc @ i)
		       end
  in
      aux(lst, [])
  end


(* 1 (d) *)
fun similar_names(lst: string list list, full_name: {first:string,middle:string,last:string}) =
  let
      fun first {first:string,middle:string,last:string} = first
      fun create_records (subslst,acc) = 
	case subslst of
	    [] => full_name::acc
	  | x::xs' =>  let val recName =
			       case full_name of
				   {first=a,middle=b,last=c} => {first=x,middle=b,last=c}
		       in
			   create_records(xs', acc@[recName])
		       end
  in
      create_records(get_substitutions2(lst,first full_name),[])
  end



(* Data types provided by Dan *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* 2 (a) *)
fun card_color card =
  case card of
      (Clubs,_) => Black
    | (Spades,_) => Black
    | (Diamonds,_) => Red
    | (Heats,_) => Red

(* 2 (b) *)
fun card_value card =
  case card of
      (_, Num i) => i
    | (_, Ace) => 11
    | (_, _) => 10

(* 2 (c) *)
fun remove_card (cards, card, ex) =
  case cards of
      [] => raise ex
    | x::xs' => if x = card then xs' else x::remove_card(xs', card, ex)

(* 2 (d) *)
fun all_same_color cards =
  case cards of
      [] => true
    | x::[] => true
    | x::y::tl => if card_color(x) = card_color(y) then all_same_color(y::tl) else false


(* 2 (e) *)
fun sum_cards cards =
  let fun aux(cards, acc) =
	case cards of
	    [] => acc
	  | x::xs' => aux(xs', card_value(x) + acc)
  in
      aux(cards, 0)
  end
      
(* 2 (f) *)
fun score (cards, goal) =
  let
      val sum = sum_cards(cards)
  in
      if sum > goal
      then if all_same_color(cards)
	   then (3 * (sum - goal)) div 2
	   else (3 * (sum - goal))
      else
	  if all_same_color(cards)
	  then (goal - sum) div 2
	  else (goal - sum)
  end
      
(* 2 (g) *)
fun officiate (cards, moves, goal) =
  let
      fun state (cards, held_cards, moves) =
	case (cards, held_cards, moves) of
	    (_, _, []) => score(held_cards, goal)
	 |  ([], _, Draw::tlMoves) => score(held_cards, goal)
	 | (hdCard::tlCards, _, Draw::tlMoves) => if sum_cards(hdCard::held_cards) > goal
						  then score(hdCard::held_cards, goal)
						  else state(tlCards, hdCard::held_cards, tlMoves)
	 | (_, _, (Discard c)::tlMoves)  => state(cards, remove_card(held_cards, c, IllegalMove), tlMoves) 
  in
      state(cards, [], moves)
  end
