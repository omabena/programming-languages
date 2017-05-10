fun n_times (f, n, x)=
  if n = 0
  then x
  else f (n_times(f, n-1, x))

fun increment x = x +1
fun double x = x + x
val x1 = n_times(double, 4, 7)
val x2 = n_times(increment, 4, 7)
val x3 = n_times(tl, 2, [4, 8, 12, 16])


fun times_until_zero (f, x) =
  if x = 0 then 0 else 1 + times_until_zero(f, f x)
					   
							      
