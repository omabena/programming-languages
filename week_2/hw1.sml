fun is_older(date1 : int * int * int, date2 : int * int * int) =
  if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 = #3 date2 then false
  else if #1 date1 < #1 date2 then true
  else if #1 date1 <> #1 date2 then false				      
  else if #2 date1 < #2 date2 then true
  else if #2 date1 <> #2 date2 then false					
  else if #3 date1 < #3 date2 then true
  else false

	       		      
fun number_in_month (dates : (int*int*int) list, month : int) =
  let
      fun count(dates: (int*int*int) list, number) =
	if null dates
	then number
	else	    
	    if #2 (hd dates) = month
	    then count(tl dates, number + 1)
		      
	    else count(tl dates, number)		     
  in
      count(dates, 0)
  end

fun number_in_months(dates : (int*int*int) list, months : int list) =
  let
      fun count(months, number) =
	if null months
	then number
	else count(tl months, number + number_in_month(dates, hd months))
  in
      count(months, 0)
  end

fun dates_in_month(dates : (int*int*int) list, month : int) =
  let
      fun appendDates(dates : (int*int*int) list, monthDates : (int*int*int) list) =
	if null dates
	then monthDates
	else
	    if #2 (hd dates) = month
	    then appendDates(tl dates, monthDates @ [hd dates])
	    else appendDates(tl dates, monthDates)
  in
      appendDates(dates, [])
  end
      

fun dates_in_months(dates : (int*int*int) list, months : int list) =
  let
      fun appendDates(months : int list, monthDates : (int*int*int) list) =
	if null months
	then monthDates
	else appendDates(tl months, monthDates @  dates_in_month(dates, hd months))
  in
      appendDates(months, [])
  end

fun get_nth(phrase : string list, nth : int) =
  let
      fun transverse_index(phrase: string list, index : int) =
	if index = nth
	then hd phrase
	else transverse_index(tl phrase, index + 1)
  in
      transverse_index(phrase, 1)
  end

fun date_to_string(year : int, month : int, day : int) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
      fun transform_date() =
	get_nth(months, month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString year
  in
      transform_date()
  end

fun number_before_reaching_sum (sum : int, sumList : int list) =
  if null sumList then 1
  else if hd sumList >= sum then 0
  else 1 + number_before_reaching_sum (sum - hd sumList, tl sumList) 
	
fun what_month(dayNumber : int) =
      1 + number_before_reaching_sum(dayNumber,  [31, 28, 31, 30,  31, 30, 31, 31, 30, 31, 30, 31])

				    
fun month_range(day1 : int, day2 : int) =
  if day1 > day2 then []
  else what_month day1 :: month_range(day1 + 1, day2)
				 
fun oldest(dateList: (int*int*int) list) =
  if null dateList
  then NONE
  else let
      fun oldest_notempty(dateList : (int*int*int) list) =
	if null (tl dateList)
	then hd dateList
	else let val oldest_ans = oldest_notempty(tl dateList)
	     in
		 if is_older(hd dateList, oldest_ans)
		 then hd dateList
		 else oldest_ans
	     end
  in
      SOME(oldest_notempty dateList)
  end
	   
