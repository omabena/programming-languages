(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun all_except_option (name, names) =
	case names of
		[] => NONE
	 | n::ns => (case (same_string(n, name), all_except_option(name, ns)) of
						 (true, SOME ns') => SOME ns'
					  | (false, SOME ns') => SOME (n::ns')
					  | (true,NONE) => SOME ns
					  | (false, NONE) => NONE)

fun get_substitutions1 (subs, s) =
	case subs of
		[] => []
	  | sub::subs' => (case all_except_option (s, sub) of
						  NONE => get_substitutions1(subs',s)
						| SOME ss => ss @ get_substitutions1(subs',s))

fun get_substitutions2 (subs, s) =
	let fun helper (ss, acc) =
			case ss of
				[] => []
			  | (s'::ss') => if same_string(s, s')
							then acc@ss'
							else helper(ss', s'::acc)
	in
		case subs of
			[] => []
		 |  sub::subs' => helper(sub, []) @ get_substitutions2(subs', s)
	end

fun similar_names (subs, name) =
	case name of
		{first= f, middle= m, last=l} =>
		let
			val subs' = get_substitutions1(subs, f)
			fun helper(ss) =
				case ss of
					[] => []
				  | s::ss' => {first=s, middle = m, last = l}::helper(ss')
		in
			name::helper(subs')
		end
			
fun card_color card =
	case card of
		(Clubs, _) => Black 
	 | (Spades, _) => Black
	 | (Diamonds, _) => Red
	 | (Hears, _) => Red

fun card_value card =
	case card of
		(_, Num i) => i
	 | (_, Ace) => 11
	 | _ => 10

fun remove_card (cs, c, e) =
	case cs of
		[] => raise e
	 | c'::cs' => if c = c'
				  then cs'
				  else remove_card(cs', c, e)
					
fun all_same_color cs =
	let
		fun helper (cs', color) =
			case cs' of
				[] => true
			  | c::cc => card_color(c) = color andalso helper(cc, color)
	in
		case cs of
			c::cc => helper(cc, card_color(c))
		 | _ => true
	end
		
fun sum_cards cs =
	let fun helper(cs', acc) =
			case cs' of 
				[] => acc
			  | c::cc => helper(cc, acc + card_value(c))
	in
		helper(cs, 0)
	end


fun score (cs, goal) =
	let
		val sum = sum_cards(cs)
		val prelim =
			if sum > goal
			then 3*(sum - goal)
			else goal-sum
	in
		if all_same_color(cs)
		then prelim div 2
		else prelim
	end

fun officiate (cs, ms, goal) =
	let
		fun helper (h,cs, ms) =
			case ms of
				Draw::ms' => (case cs of
							[] => score(cs, goal)
							  | c::cs' => if sum_cards(c::h) > goal
										  then score(c::h, goal)
										  else helper(c::h, cs', ms')
							 )
			  | Discard c::ms' => helper(remove_card(h,c,IllegalMove),cs,ms')
			  | [] => score(h,goal)
	in
		helper([],cs,ms)
	end

(* fun sum_cards_challenge cs =
	let fun add_all (ns, v) =
		case ns of
			n::ns' => (n+v)::add_all(ns',v)
		  | [] => []
		fun helper (cs, ns) =
			case cs of
				[] => ns
			  | (_, Ace)::cs' => helper(cs',add_all(ns, 1) @ add_all(ns, 11))
			  | c::cs' => helper(cs',add_all(ns, card_value(c)))
					  
	in
		helper(cs, [0])
	end

fun score_challenge (cs, goal) =
	let
		val sums = sum_cards_challenge(cs)
		fun min (i1, i2) = if i1 > i2 then i1 else i2				
		fun helper (sums,acc) =
			case sums of
				[] => acc
			  | sum::sums' => 
				helper(sums', min((if sum > goal
								  then 3*(sum - goal)
								  else goal-sum),
								  acc))
		val prelim = min_list(helper(sums, [0]))
	in
		if all_same_color(cs)
		then prelim div 2
		else prelim
	end
*)
