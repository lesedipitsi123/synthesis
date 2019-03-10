module Synthesis

let abelar x =
    match x > 12 && x < 3097 && (x % 12) = 0 with
    | true -> true
    | _ -> false
let area x y =
    match x >= 0.0 && y >= 0.0 with
    | true -> 0.5 * x * y
    | _ -> failwith "Invalid Input"

let zollo i =
    match i >=0 with
    | true -> 2 * i
    | _ -> i * -1

let min a b =
    match a < b with
    | true -> a
    | _ -> b
    
let max a b =
    match a > b with
    | true -> a
    | _ -> b

let ofTime h m s = h * 60*60 + m *60 + s

let toTime s =
   let h = s/3600
   let m = (s/60) % 60
   let sec = (s - (h * 3600)) % 60
   match s < 0 with
   | true -> (0, 0, 0)
   | _ ->  (h, m, sec)
   

let digits d =
    let rec countDigits i acc =
        match i / 10 = 0 with
        | true -> acc 
        | _ -> countDigits (i/10) (acc + 1)
    countDigits d 1
        
        
let minmax (a,b,c,d) =
     min (min a b) (min c d) , max (max a b) (max c d)

let isLeap year =
    match year < 1582 with
    | true -> failwith "Invalid Input"
    | _ -> ((year % 4 = 0) && year % 100 <> 0) || ((year % 4 =0 && year % 400 = 0))

let month m =
   match m with 
   | 1 -> "January", 31
   | 2 -> "February", 28
   | 3 -> "March", 31
   | 4 -> "April", 30
   | 5 -> "May", 31
   | 6 -> "June", 30
   | 7 -> "July", 31
   | 8 -> "August", 31
   | 9 -> "September", 30
   | 10 -> "October", 31
   | 11 -> "November", 30
   | 12 -> "December", 31
   | _ -> failwith "Invalid Input"

let toBinary b =
    match b < 0 with
    | true -> failwith "Invalid Input"
    | _ ->   
    let rec convert d acc =
        match d = 0 with
        | true -> 
            match acc = "" with
            | true -> "0"
            | _ -> acc
        | _ -> 
            match d % 2 = 0 with
            | true -> convert (d / 2) ("0" + acc)
            | false -> convert (d / 2) ("1" + acc)
    convert b "" 

let bizFuzz n =
    let rec countDiv i c1 c2 c3 =
        match i = (n + 1) || n < 0 with
            | true -> c1, c2, c3 
            | _ -> 
                match i % 3 = 0, i % 5 = 0, (i % 3 = 0 && i % 5 = 0) with
                | true, true, true -> countDiv (i + 1) (c1 + 1) (c2 + 1) (c3 + 1)
                | true, false, false -> countDiv (i + 1) (c1 + 1) c2 c3
                | false, true, false -> countDiv (i + 1) c1 (c2 + 1) c3
                | _ -> countDiv (i + 1) c1 c2 c3               
    countDiv 1 0 0 0

let monthDay d y =
    match isLeap y with
    | true -> 
        match month (d/30) with
        | m, day -> m
    | false ->
         match month (d/30) with
            | m, day -> m

let coord _ =
    failwith "Not implemented"