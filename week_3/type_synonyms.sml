datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | king | Ace | Num of int

type card = suit * rank

type name_record = { stydent_num : int option,
		     first       : string,
		     middle      : string,
		     last        : string }

fun is_Queen_of_Spades (c : card) =
  #1 c = Spade andalso #2 c= Queen

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3 = (Spade, Ace)			   
