open System
open System.IO

type IntCode = int[]

exception InvalidInstruction of int

let run (code: IntCode) =
    let mutable pc = 0
    while code.[pc] <> 99 do
        let operator =
            match code.[pc] with
            | 1 -> (+)
            | 2 -> (*)
            | _ -> raise (InvalidInstruction(code.[pc]))
        let left = code.[code.[pc + 1]]
        let right = code.[code.[pc + 2]]
        let result = operator left right
        code.[code.[pc + 3]] <- result
        pc <- pc + 4

let runWith (code: IntCode) noun verb =
    code.[1] <- noun
    code.[2] <- verb
    run code

let findNounAndVerb (code: IntCode) expectedOutput =
    let mutable result = None
    for noun in 0..99 do
        for verb in 0..99 do
            let code = Array.copy code
            runWith code noun verb
            if code.[0] = expectedOutput then
                result <- Some((noun,verb))
    result

let loadIntCode filePath: IntCode =
    (File.ReadAllText filePath).Split(',')
    |> Array.map Int32.Parse
   
[<EntryPoint>]
let main argv =
    // Part 1
    let intCode = loadIntCode "Input.txt"

    runWith intCode 12 2

    printfn "Value at position 0: %A" intCode.[0]

    // Part 2
    let intCode = loadIntCode "Input.txt"

    let nounAndVerb =
        findNounAndVerb intCode 19690720
        |> Option.map (fun x -> sprintf "%02i%02i" (fst x) (snd x))

    printfn "Noun and verb: %A" nounAndVerb

    0
