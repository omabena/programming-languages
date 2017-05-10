(* Coursera Programming Languages, Homework 3, Provided Code *)

(* 1. *)
fun only_capitals stringList =
  List.filter (fn x => Char.isUpper(String.sub(x,0)))  stringList

(* 2. *)	      
fun longest_string1 stringList =
  List.foldl (fn (x,y) => if String.size x > String.size y  then x else y) "" stringList 

(* 3. *)	     
fun longest_string2 stringList =
  List.foldl(fn (x,y) => if String.size x < String.size y then y else x) "" stringList
	    
fun longest_string_helper f xs =
  List.foldl f "" xs

(* 4a. *)	     
fun longest_string3 stringList =
  let
      val f = fn(x,y) => if String.size x > String.size y then x else y
  in
      longest_string_helper f stringList
  end

(* 4b. *)      
fun longest_string4 stringList =
  let
      val f = fn(x,y) => if String.size x < String.size y then y else x
  in
      longest_string_helper f stringList
  end

(* 5. *)      
fun longest_capitalized i  = (longest_string3 o only_capitals) i

(* 6. *)							       
fun rev_string i =  (String.implode o List.rev o String.explode) i
	    
exception NoAnswer

(* 7. *)	      
fun first_answer f l =
  case l  of
      [] => raise NoAnswer
    | x::xs' => case f x of
		    SOME i => i
		  | NONE => first_answer f xs'  

(* 8. *)					 
fun all_answers f l =
  let fun helper_f f l acc  =
	case l of
	    [] => SOME acc
	  | x::xs' =>  case f x of
			   SOME i => helper_f f xs' (acc@i)
			 | NONE => NONE
  in
      helper_f f l []
  end
      				       					 
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* 9a. *)
fun count_wildcards p = g (fn () => 1) (fn x => 0) p

(* 9b. *)
fun count_wild_and_variable_lengths p =
  count_wildcards p + (g (fn () => 0) (fn x => String.size x) p)

(* 9c. *)			  
fun count_some_var (s: string, p: pattern) =
  g (fn () => 0) (fn x => if s = x then 1 else 0) p

(* 10 *)
fun check_pat p =
  let
      fun get_variables p acc =
	  case p of
	      Variable x        => [x] @ acc
	    | TupleP ps         => List.foldl (fn (p, acc) => (get_variables p acc))[] ps
	    | ConstructorP(_,p) => get_variables p acc
	    | _                 => acc
				       
      fun check_distinct l =
	case l of
	    [] => true
	  | x::[] => true
	  | x::y::xs' => if x = y then false else check_distinct(y::xs')
  in
      check_distinct(get_variables p [])
  end  

(* 11 *)
fun match (v, p) =
  case (v,p) of
      (_, Wildcard) => SOME []
    | (_, Variable x) => SOME [(x,v)]
    | (Unit, UnitP) => SOME[]
    | (Const i, ConstP x) => if i = x then SOME [] else NONE
    | (Tuple vs, TupleP ps) => if (List.length ps = List.length vs) then all_answers (match) (ListPair.zip(vs,ps)) else NONE
    | (Constructor(s2, v), ConstructorP(s1,p)) => if s1 = s2 then match(v,p) else NONE
    | _ => NONE		

(* 12 *)
fun first_match v l =
  SOME (first_answer (fn p => match(v,p)) l)
  handle NoAnswer => NONE
			 

      

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
