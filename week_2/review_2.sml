(*compares dates, returns true if first date is older*)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
  if #1 date1 = #1 date2
  then
      if #2 date1 = #2 date2
      then #3 date1 < #3 date2
      else #2 date1 < #2 date2
  else #1 date1 < #1 date2

(*recursively iterate through list counting every month that is the same as the argument*)                     
fun number_in_month (dates : (int*int*int) list, month : int) =
  if null dates then 0
  else
      if #2 (hd dates) = month
      then 1 + number_in_month(tl dates, month)
      else number_in_month(tl dates, month)

(*recursively iterate through months list adding up each time a month is the same as a month in the dates list*)
fun number_in_months (dates : (int*int*int) list, months : int list) =
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*recursively go through dates list adding to a list all dates that are in given month*)                                                           
fun dates_in_month (dates : (int*int*int) list, month : int) =
  if null dates then []
  else
      if #2 (hd dates) = month
      then hd dates::dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)

(*recursively go through months list adding to a list all dates that are in each month*)			 
fun dates_in_months (dates : (int*int*int) list, months : int list) =
  if null months then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*gets the nth element in a list by recursively calling function n number of times*) 
fun get_nth (a : string list, n : int) =
  if null a then "no list"
  else if n = 1
  then hd a
  else get_nth(tl a, n-1)

(*gets month name from local months list using get_nth function. Convert day and year to string and reformat*)              
fun date_to_string (date : int*int*int) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in get_nth(months, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end

(*uses a local helper function to sum elements of list up to the point where if the next element was added the sum would be greater than the sum in the first argument*)
fun number_before_reaching_sum (sum : int, numbers : int list) =
  if null numbers then 0
  else
      let fun count (numbers : int list, a : int, n : int) =
	    if hd numbers + a >= sum then n
	    else count(tl numbers, hd numbers + a, n + 1)
      in count(numbers, 0, 0)
      end

(*create list of how many days are in each month. then uses previous function to determine what month the given day is in*)          
fun what_month (day : int) =
  let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in 1 + number_before_reaching_sum(day, days_in_months)
  end

(*recursively call function adding 1 to day1 until it is eaqual to day2, get month of each day by using what_month function and add each result to a list*)      
fun month_range (day1 : int, day2 : int) =
  if day1 > day2 then []
  else what_month(day1)::month_range(day1 + 1, day2)

(*recusively go through date list using is_older function on each date storing the current oldest date in ans*)                                    
fun oldest (dates : (int*int*int) list) =
  if null dates then NONE
  else
      let val ans = oldest(tl dates)
      in
          if isSome ans andalso is_older(valOf ans, hd dates) then ans
          else SOME (hd dates)
      end

(*helper function -- uses local delete function that recursively add each element to a new list only if that element does not already exist.*)               
fun remove_duplicates (months : int list) =
  if null months then []
  else
      let fun delete (n : int, alist : int list) =
            if null alist then []
            else
                if n = hd alist then delete(n, tl alist)
                else hd alist::delete(n, tl alist)
      in hd months::remove_duplicates(delete(hd months, tl months))
      end
(*uses reomve_duplicates and number_in_months functions*)
fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
  number_in_months(dates, remove_duplicates(months))

(*uses reomve_duplicates and dates_in_months functions*)                  
fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
  dates_in_months(dates, remove_duplicates(months))

(*uses local helper functions and lists to determine if day is valid for the month. Checks month is value between 1 and 12 and year is greater than 0*)             
fun reasonable_date (date : int*int*int) =
  let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
      val days_in_months_leap = [31,29,31,30,31,30,31,31,30,31,30,31]

      fun get_nth (a : int list, n : int) =
        if n = 1 then hd a
        else get_nth(tl a, n-1)

      fun day_check (day : int, month : int, is_leap : bool) =
        if is_leap
        then day <= get_nth(days_in_months_leap, month) andalso day > 0
        else day <= get_nth(days_in_months, month) andalso day > 0

      fun leap_check (year : int) =
        year mod 400 = 0 orelse year mod 4 = 0 andalso year mod 100 <> 0
  in
      #1 date > 0 andalso #2 date > 0 andalso #2 date < 13 andalso day_check(#3 date, #2 date, leap_check(#1 date))
  end
      
                                                            
                               
           
	    
	    

				   
				  

	  
	  

      
