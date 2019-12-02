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

let loadIntCode filePath: IntCode =
    (File.ReadAllText filePath).Split(',')
    |> Array.map Int32.Parse
   
[<EntryPoint>]
let main argv =

    let intCode = loadIntCode "Input.txt"

    // restore 1202 program state
    intCode.[1] <- 12
    intCode.[2] <- 2

    run intCode |> ignore
    
    let result = intCode.[0]

    printfn "Value at position 0: %A" result
    
    0
