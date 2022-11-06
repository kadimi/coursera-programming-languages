(* Start of date_to_days *)
fun date_to_days(date : int * int * int) =
    (#1 date * 365) + (#2 date * 12) + (#3 date)
(* End of date_to_days *)


(* Start of 1 *)
fun is_older (a: int * int * int, b: int * int * int) =
    date_to_days a < date_to_days b
(* End of 1 *)


(* Start of 2 *)
fun number_in_month( dates : (int * int * int) list, month : int ) =
    if null dates
    then 0
    else 
        if #2 (hd dates) = month then 1 else 0
        + number_in_month(tl dates, month)
(* End of 2 *)


(* Start of 3 *)
fun number_in_months( dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else
        number_in_month(dates, hd months)
        + number_in_months(dates, tl months)

(* End of 3 *)


(* Start of 4 *)
fun dates_in_month( dates: (int * int * int) list, month : int) =
    if null dates
    then []
    else 
        if number_in_month([hd dates], month) = 1
        then hd dates :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* End of 4 *)


(* Start of 5 *)
fun dates_in_months( dates: (int * int * int) list, months: int list) = 
    if null months
    then []
    else 
        dates_in_month(dates, hd months)
        @ dates_in_months(dates, tl months)

(* End of 5 *)


(* Start of 6 *)
fun get_nth ( words: string list, n: int) =
    if n = 1
    then hd words
    else get_nth(tl words, n - 1)

(* End of 6 *)


(* Start of 7 *)
fun date_to_string( year: int, month: int, day: int ) =
    let
        val months = [
            "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"
        ]
        val year  = Int.toString(year)
        val month = get_nth(months, month)
        val day   = Int.toString(day)
    in
        month ^ " " ^ day ^ ", " ^ year 
    end

(* End of 7 *)


(* Start of 8 *)
fun number_before_reaching_sum(sum: int, numbers: int list) =
    if null numbers
    then 0
    else
        let
            fun add_with_limit(current_total: int, numbers: int list, tracker: int) = 
                let val new_total = current_total + hd numbers
                in
                    if (new_total >= sum)
                    then tracker
                    else add_with_limit(new_total, tl numbers, tracker + 1)
                end
        in
            add_with_limit(0, numbers, 0)
        end


(* End of 8 *)


(* Start of 9 *)
fun what_month(day: int) = 1 + number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])
(* End of 9 *)


(* Start of 10 *)
fun month_range(day1: int, day2: int) =
    if day1 > day2 then [] else what_month day1 :: month_range(day1 + 1, day2)
(* End of 10 *)


(* Start of 11 *)
fun oldest(dates: (int * int * int) list) =
    if null dates
    then NONE
    else 
        let
            fun oldest_of_two_dates(a: int * int * int, b: int * int * int) = if is_older(a, b) then a else b
            fun oldest_vs_dates(date: int * int * int, dates: (int * int * int) list) =
                if null dates
                then date
                else oldest_vs_dates( oldest_of_two_dates(date, hd dates), tl dates )
        in
            SOME(oldest_vs_dates(hd dates, tl dates))
        end
(* End of 11 *)


(* Start of list_unique *)
fun list_unique(list: int list) =
    let
        fun list_has(list: int list, value: int) =
            not(null list)
            andalso(
                hd list = value
                orelse
                list_has(tl list, value)
            )
    in
        if null list then []
        else 
            if not(list_has(tl list, hd list))
            then hd list :: list_unique(tl list)
            else list_unique(tl list)
    end
(* End of list_unique *)
