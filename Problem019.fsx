open System

type Day = 
    | Monday 
    | Tuesday 
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday

let nextDay = function  
    | Monday -> Tuesday 
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Saturday
    | Saturday -> Sunday
    | Sunday -> Monday

type Month =
    | January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December

type Year = int

let nextMonth (month : Month, year : Year) =
    let nextMonth = 
        match month with
        | January -> February 
        | February -> March
        | March -> April
        | April -> May
        | May -> June
        | June -> July
        | July -> August
        | August -> September
        | September -> October
        | October -> November
        | November -> December
        | December -> January
    let newYear = if nextMonth = January then year + 1 else year
    (nextMonth, newYear)

type Date = int

type FullDate = Day * Date * Month * Year 

let isLeap year =
    year % 4 = 0 && (year % 100 <> 0 || year % 400 = 0) 

let daysInMonth (month : Month) isLeap = 
    match month with
    | April | June | September | November -> 30
    | February -> if isLeap then 29 else 28
    | _ -> 31 

let nextDate (day, date, month, year) =
    if date < daysInMonth month (isLeap year) then 
        (nextDay day, date + 1, month, year)
    else 
        let (newMonth, newYear) = nextMonth (month, year)    
        in (nextDay day, 1, newMonth, newYear)

let allDates = 
    Seq.unfold (fun d -> 
        let next = nextDate d in Some(d,next)) (Monday, 1, January, 1900)

let allDatesBetween first afterLast =
    allDates 
    |> Seq.skipWhile (fun (_, date, month, year) -> (date, month, year) <> first)
    |> Seq.takeWhile (fun (_, date, month, year) -> (date, month, year) <> afterLast)
    
let euler019 = 
    allDatesBetween (1, January, 1901) (1, January, 2001)
    |> Seq.filter (fun (day, date, _, _) -> day = Sunday && date = 1)
    |> Seq.length

Console.WriteLine euler019