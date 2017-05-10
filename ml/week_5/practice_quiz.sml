fun increment x = x + 1;

fun map x y = List.map x y;

fun foo f xs = 1 +
               foldr (fn (x,y) => x * (y+1))  0 (map f xs)

		     

fun bar xs = if xs = []
	     then 0
	     else 1 + bar xs


val x = 50
val y = 3
val z = 10
val f = fn z => z
val a =
    let
	val x = 3*x
	val z = y*z
    in
	x*z
    end
fun f x z = x + y + z


fun foo f x y z =
  if x >= y
  then (f z)
  else foo f y x (tl z)

fun baz f a b c d e = (f (a ^ b))::(c + d)::e

fun maybeEven x =
  if x = 0
  then true
  else
      if x = 50
      then false
      else maybeOdd (x-1)

and maybeOdd y =
    if y = 0
    then (print("!"); false
	      )
    else
	(
	  print("!");
	if y = 99
	then true
	else maybeEven (y-1)
		       )
		       


		       
