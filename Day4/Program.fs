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

let rec compairPairs cmpFn (digits: int list)  =
    if digits.Length < 2 then
        false
    elif cmpFn digits.[0] digits.[1] then
        true
    else
        digits
        |> List.splitAt 1
        |> snd
        |> compairPairs cmpFn

let hasSequentialDigis digits =
    compairPairs (fun a b -> a = b) digits

let hasIncreasingDigits digits =
    compairPairs (fun a b -> a > b) digits
    |> not

let validate password =
    let digits = split password
    hasSequentialDigis digits &&
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
    

    0
