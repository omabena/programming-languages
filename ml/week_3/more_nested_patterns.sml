fun nodecreasing xs =
  case xs of
      [] => true
    | _::[] => true
    | head::(neck::rest) => head <= neck andalso nodecreasing (neck::rest)

datatype sgn = P | N | Z

fun multsign (x1,x2) =
  let fun sign x = if x = 0 then Z else if x > 0 then P else N
  in
      case (sign x1, sign x2) of
	  (Z, _) => Z
	| (_,Z) => Z
	| (P,P) => P
	| _ => N
  end

fun len xs =
  case xs of
      [] => 0
    | _::xs' => 1 + len xs'
			

fun f(a::b::c) = 2 + f c
  | f [] = 0
  | f (a) = 10 

val x = f [1,2,3]
