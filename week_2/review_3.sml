
fun get_day (d : int * int * int) =
  let
    val mondays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    fun sum_list (xs : int list, len : int) =
      if null xs orelse len = 0
      then 0
      else hd xs + sum_list(tl xs, len - 1)
  in
    ((#1 d) - 1) * 365 + sum_list(mondays, (#2 d) - 1) + (#3 d)
  end

fun is_older (d1 : int * int * int, d2 : int * int * int) =
  get_day(d1) < get_day(d2)

fun number_in_month (dlist : (int * int * int) list, monday : int) =
  if null dlist
  then 0
  else
    if (#2 (hd dlist)) = monday
    then 1 + number_in_month(tl dlist, monday)
    else number_in_month(tl dlist, monday)

fun number_in_months (dlist : (int * int * int) list, mondays : int list) =
  if null mondays
  then 0
  else
    number_in_month(dlist, hd mondays) + number_in_months(dlist, tl mondays)

fun dates_in_month (dlist : (int * int * int) list, monday : int) =
  if null dlist
  then []
  else
    if (#2 (hd dlist)) = monday
    then (hd dlist) :: dates_in_month(tl dlist, monday)
    else dates_in_month(tl dlist, monday)

fun dates_in_months (dlist : (int * int * int) list, mondays : int list) =
  if null mondays
  then []
  else
    dates_in_month(dlist, hd mondays) @ dates_in_months(dlist, tl mondays)

fun get_nth (slist : string list, nth : int) =
  if nth = 1
  then hd slist
  else
    get_nth(tl slist, nth - 1)

fun date_to_string(d : int * int * int) =
  let
    val month_names = [
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    ]
  in
    get_nth(month_names, (#2 d)) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
  end

fun number_before_reaching_sum(sum : int, vals : int list) =
  let
    fun iter(nth : int, sum : int, vals : int list) =
      if sum <= 0
      then nth - 1
      else
        iter(nth + 1, sum - hd vals, tl vals)
  in
    iter(0, sum, vals)
  end

fun what_month(nth_day_of_year : int) =
  let
    val mondays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(nth_day_of_year, mondays) + 1
  end

fun month_range(nth_day1_of_year : int, nth_day2_of_year : int) =
  if nth_day1_of_year > nth_day2_of_year
  then []
  else
    (what_month nth_day1_of_year) :: month_range(nth_day1_of_year + 1,
     nth_day2_of_year)

fun oldest(dlist : (int * int * int) list) =
  if null dlist
  then NONE
  else
    let
      fun oldest_noempty(dlist : (int * int * int) list) =
        if null (tl dlist)
        then hd dlist
        else 
          let
            val tl_ans = oldest_noempty(tl dlist)
          in
            if is_older(hd dlist, tl_ans)
            then hd dlist
            else tl_ans
          end
    in
      SOME (oldest_noempty dlist)
    end

fun is_in(month : int, months : int list) =
  if null months
  then false
  else
    month = hd months orelse is_in(month, tl months)

fun remove_duplicates(months: int list) =
  if null months
  then []
  else
    if is_in(hd months, tl months)
    then remove_duplicates(tl months)
    else (hd months) :: remove_duplicates(tl months)

fun number_in_months_challenge(dlist : (int * int * int) list, months : int
  list) =
  number_in_months(dlist, remove_duplicates months)

fun dates_in_months_challenge(dlist : (int * int * int) list, months: int
  list) =
  dates_in_months(dlist, remove_duplicates months)

fun reasonable_date(d : int * int * int) =
  let
    val mondays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val year = (#1 d)
    val month = (#2 d)
    val day = (#3 d)
    fun is_leap() =
      (year mod 400) = 0 orelse ((year mod 4) = 0 andalso (year mod 100) <> 0)

    fun get_nth(ilist : int list, nth : int) =
      if nth = 1
      then hd ilist
      else
        get_nth(tl ilist, nth - 1)

    fun get_mondays() =
      if is_leap() andalso month = 2
      then get_nth(mondays, month) + 1
      else get_nth(mondays, month)

    fun not_reasonable_day() =
      let
        val max_day = get_mondays()
      in
        day <= 0 orelse day > max_day
      end
  in
    if year <= 0 orelse month <= 0 orelse month > 12 orelse not_reasonable_day()
    then false
    else true
  end
