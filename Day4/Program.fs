open System

let split digit =
    let n = (int)(floor(log10((float)digit)) + 1.0)
    seq { 1..n }
    |> Seq.mapFold
        (fun rest _ ->
            (rest % 10, rest / 10))
            digit
    |> fst
    |> Seq.rev
    |> List.ofSeq

let rec comparePairs cmpFn (digits: int list)  =
    if digits.Length < 2 then
        false
    elif cmpFn digits.[0] digits.[1] then
        true
    else
        digits
        |> List.skip 1
        |> comparePairs cmpFn

let hasSequentialDigis digits =
    comparePairs (fun a b -> a = b) digits

let hasIncreasingDigits digits =
    comparePairs (fun a b -> a > b) digits
    |> not

let validate password =
    let digits = split password
    hasSequentialDigis digits &&
    hasIncreasingDigits digits

// Yuck :( :(
let hasGroupOfTwo (digits: int list) =
    let mutable group = false
    let mutable prev = -1
    let mutable count = 1
    for x in digits do
        if x = prev then
            count <- count + 1
        elif count = 2 then
            group <- true
            count <- 1
        else
            count <- 1
        prev <- x 
    if count = 2 then true else group

let validate2 password =
    let digits = split password
    hasGroupOfTwo digits &&
    hasIncreasingDigits digits

[<EntryPoint>]
let main argv =

    let passwordRange = seq { 165432..707912 }

    let validPasswordCount =
        passwordRange
        |> Seq.map validate
        |> Seq.filter (fun x -> x)
        |> Seq.length
     
    printfn "Number of valid passwords: %A" validPasswordCount
    
    let validPasswordCount2 =
        passwordRange
        |> Seq.map validate2
        |> Seq.filter (fun x -> x)
        |> Seq.length

    printfn "Number of valid passwords 2: %A" validPasswordCount2

    0
