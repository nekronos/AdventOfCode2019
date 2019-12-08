open System
open System.IO
open System.Linq

type IntCode = int[]

exception InvalidInstruction of int
exception InvalidParameterMode of int

type Opcode =
    | Add
    | Multiply
    | Read
    | Write
    | Halt

type ParameterMode =
    | Position
    | Immediate

type Parameter = {
    number: int
    mode: ParameterMode
}

type Instruction = {
    opcode: Opcode
    p1: Parameter
    p2: Parameter
    p3: Parameter
}

let getParamMode mode =
    match mode with
    | 0 -> Position
    | 1 -> Immediate
    | _ -> raise (InvalidParameterMode(mode))

let getPcIncrement opcode =
    match opcode with
    | Add -> 4
    | Multiply -> 4
    | Read -> 2
    | Write -> 2
    | Halt -> 0

let split digit =
    let n = (int)(floor(log10((float)digit)) + 1.0)
    seq { 1..n }
    |> Seq.mapFold
        (fun rest _ ->
            (rest % 10, rest / 10))
            digit
    |> fst
    |> Seq.rev
    |> Seq.append (Enumerable.Repeat(0, 5 - n))
    |> List.ofSeq

let decode instr =
    let digits = split instr
    let opcodeInt = (digits.[3] * 10) + digits.[4]
    let opcode =
        match opcodeInt with
        | 1 -> Add
        | 2 -> Multiply
        | 3 -> Read
        | 4 -> Write
        | 99 -> Halt
        | _ -> raise (InvalidInstruction(opcodeInt))
    let instruction = {
        opcode = opcode;
        p1 = { number = 1; mode = getParamMode digits.[2] };
        p2 = { number = 2; mode = getParamMode digits.[1] };
        p3 = { number = 3; mode = getParamMode digits.[0] };
    }
    instruction

let loadIntCode filePath: IntCode =
    (File.ReadAllText filePath).Split(',')
    |> Array.map Int32.Parse

let read (code: IntCode) pc (parameter: Parameter) =
    let index = pc + parameter.number
    let value = code.[index]
    match parameter.mode with
    | Immediate -> value
    | Position -> code.[value]

let write (code: IntCode) pc (parameter: Parameter) value =
    let index = pc + parameter.number
    match parameter.mode with
    | Immediate -> code.[index] <- value
    | Position -> code.[code.[index]] <- value
    
let run (code: IntCode) =
    let mutable pc = 0
    let mutable halt = false

    while not halt do
        let i = decode code.[pc]

        let read = read code pc
        let write = write code pc

        match i.opcode with
        | Add ->
            let a = read i.p1
            let b = read i.p2
            let c = a + b
            write i.p3 c

        | Multiply ->
            let a = read i.p1
            let b = read i.p2
            let c = a * b
            write i.p3 c

        | Read ->
            printf "Input: "
            let input = Console.ReadLine() |> Int32.Parse
            write i.p1 input

        | Write ->
            let output = read i.p1
            printf "%A" output

        | Halt -> halt <- true

        pc <- pc + (getPcIncrement i.opcode)
    done
    
[<EntryPoint>]
let main argv =

    let intCode = loadIntCode "Input.txt"

    run intCode |> ignore
    
    0
