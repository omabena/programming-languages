fun n_times (f, n, x) =
  if n = 0
  then x
  else f (n_times(f,n-1,x))
(*
n_times (f, 3, 8)
   n_times(f, 2, 8)
        n_times(f, 2, 8)
            n_times(f, 1, 8)
                f (n_times(f, 0, 8))
                8
            f(8) = 8 + 8
        f(16) = 16 + 8
    f(24) = 24 + 8
f(32) = 32 + 8 
            

*)
	 
fun double x = x + x
val x1 = n_times(double, 3, 8)

		
