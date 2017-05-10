fun fold f acc xs =
  case xs of
      [] => acc
    | x::xs' => fold f (f(acc, x)) xs'

val partial_fold = fold (fn (x,y) => x+y) 		     
val sum = fold (fn (x,y) => x+y) 0

fun exists predicate xs =
  case xs of
      [] => false
    | x::xs' => predicate x orelse exists predicate xs'

val no = exists (fn x => x=7) [3,4,5,6,8]
val yes = exists (fn x => x=7) [3,4,7,6,8]

val hasZero = exists (fn x => x=0)	
	      
