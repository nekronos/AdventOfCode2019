﻿open System
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
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
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
    | JumpIfTrue -> 3
    | JumpIfFalse -> 3
    | LessThan -> 4
    | Equals -> 4
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
        | 5 -> JumpIfTrue
        | 6 -> JumpIfFalse
        | 7 -> LessThan
        | 8 -> Equals
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
    
let run (code: IntCode) (args: int list) =
    let mutable pc = 0
    let mutable halt = false
    let mutable output = None
    let mutable argIdx = 0

    while not halt do
        let i = decode code.[pc]

        let mutable incrementPc = true

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
            let input = args.[argIdx]
            argIdx <- argIdx + 1
            write i.p1 input

        | Write ->
            output <- Some (read i.p1)

        | JumpIfTrue ->
            let x = read i.p1
            if x <> 0 then
                pc <- (read i.p2)
                incrementPc <- false

        | JumpIfFalse ->
            let x = read i.p1
            if x = 0 then
                pc <- (read i.p2)
                incrementPc <- false

        | LessThan ->
            let a = read i.p1
            let b = read i.p2
            let c =
                if a < b then
                    1
                else
                    0
            write i.p3 c

        | Equals ->
            let a = read i.p1
            let b = read i.p2
            let c =
                if a = b then
                    1
                else
                    0
            write i.p3 c

        | Halt -> halt <- true

        if incrementPc then
            pc <- pc + (getPcIncrement i.opcode)

    done
    output

let getThrusterSignal (program: IntCode) phaseSetting inputSignal =
    let args = [phaseSetting; inputSignal]
    let output = run program args
    match output with
    | Some x -> x
    | None -> raise(Exception("Unexpected output"))

let calculateThrusterSignal (program: IntCode) (phaseSettings: int list) =
    phaseSettings
    |> List.fold (
        fun input setting ->
            getThrusterSignal program setting input
        ) 0

let zeroPadSeq s =
    Enumerable.Repeat(0, 5 - (Seq.length s))
    |> Seq.append s

let toBase5 number =
    seq {
        let mutable x = number
        while x > 0 do
            yield (x % 5)
            x <- x / 5
        done
    }
    |> Seq.rev
    |> zeroPadSeq
    |> List.ofSeq

let allPhaseSettings =
    let n = (int)(Math.Pow(5.0, 5.0)) - 1
    seq { 0..n }
    |> Seq.map toBase5
    |> Seq.filter (fun x ->
        x
        |> Seq.distinct
        |> Seq.length = 5)

[<EntryPoint>]
let main argv =
    let intCode = loadIntCode "Input.txt"

    let mutable maxThrusterSignal = 0
    
    for setting in allPhaseSettings do
        let program = Array.copy intCode
        let thrusterSignal = calculateThrusterSignal program setting
        maxThrusterSignal <- max maxThrusterSignal thrusterSignal

    printfn "Max thruster signal: %A" maxThrusterSignal
    
    0
