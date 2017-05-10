fun fold (f,acc,xs) =
  case xs of
      [] => acc
    | x::xs' => fold(f, f(acc,x),xs')

fun f1 xs = fold ((fn (x,y) => x+y), 0, xs)
fun f2 xs = fold((fn (x,y) => x andalso y>=0), true, xs)

fun f3 (xs,hi,lo) =
  fold (fn (x,y) =>  x + (if y >= lo andalso y <= hi then 1 else 0),0, xs)

fun f4 (g,xs) = fold(fn (x,y) => x andalso gy), true, xs)
