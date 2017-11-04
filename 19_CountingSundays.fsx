(*
    You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
    
    How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*)

open System

let getAnswer endDate =
    let isFinished  (d: DateTime) = 
        d = endDate
    let shouldCount = fun (d: DateTime) -> d.Day = 1 && d.DayOfWeek = DayOfWeek.Sunday

    let rec calculate (date: DateTime) (sundayCount: int) =       
        let next = calculate (date.AddDays 1.0)     
        if isFinished date then
            sundayCount        

        elif shouldCount date then
            next (sundayCount + 1)

        else
            next sundayCount

    let start:DateTime  = DateTime(1901, 1, 1)
    
    calculate start 0
        
let endDateTime = DateTime(2000, 12, 31)
let answer= getAnswer endDateTime