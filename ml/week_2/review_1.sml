fun is_older (d1: int*int*int, d2: int*int*int) =
    if (#1 d1 < #1 d2) then true
    else if (#1 d1 > #1 d2) then false
    else if (#2 d1 < #2 d2) then true
    else if (#2 d1 > #2 d2) then false
    else if (#3 d1 < #3 d2) then true
    else false

fun number_in_month(l: (int*int*int) list, m: int) =
    if null l then 0
    else let val tans = number_in_month(tl l, m) in
    	 if #2 (hd l) = m then tans+1
	 else tans
	 end

fun number_in_months(l: (int*int*int) list, m: int list) =
  if null m then 0
  else number_in_months(l, tl m) + number_in_month(l, hd m)

fun dates_in_month(l: (int*int*int) list, m: int) =
  if null l then []
  else let val tans = dates_in_month(tl l, m) in
	if #2 (hd l) = m then hd l :: tans
	else tans
       end
						
fun dates_in_months(l: (int*int*int) list, m: int list) =
  if null m then []
  else dates_in_month(l, hd m) @ dates_in_months(l, tl m)

fun get_nth(l: string list, n: int) = if n=1 then hd l else get_nth(tl l, n-1)

fun date_to_string(d: int*int*int) = let val months=["January", "February", "March", "April",
						     "May", "June", "July", "August", "September", "October", "November", "December"]
in get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)

end

fun number_before_reaching_sum (s: int, l: int list) =
  let fun check(sum: int, l: int list, idx: int) =
	     if sum + hd l >= s then idx
	     else check(sum+hd l, tl l, idx+1)
  in
      check(0,l,0)
  end
      
fun what_month(d: int) = number_before_reaching_sum(d, [31,28,31,30,31,30,31,31,30,31,30,31])+1

												  
fun month_range(d1: int, d2: int) =
  if d1 > d2 then []
  else what_month(d1) :: month_range(d1+1,d2)

fun oldest(l: (int*int*int) list) =
  if null l then NONE
  else let fun irec(ans: (int*int*int), l: (int*int*int) list) =
	     if null l then ans
	     else irec(if is_older(ans, hd l) then ans else hd l ,tl l)
       in
	   SOME(irec(hd l, tl l))
       end
	   
	      
