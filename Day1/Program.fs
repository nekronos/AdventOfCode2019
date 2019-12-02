open System
open System.IO

let fuelReq mass =
    mass / 3 - 2 |> max 0

let rec totalFuelReq mass =
    match mass with
    | 0 -> 0
    | _ ->
        let req = fuelReq mass
        req + (totalFuelReq req)

let calcFuelReq modules calcFn =
    modules
    |> Array.map calcFn
    |> Array.reduce (+)

let loadModules filePath =
    File.ReadAllLines filePath
    |> Array.map Int32.Parse

[<EntryPoint>]
let main argv =

    let modules = loadModules "Input.txt"

    let fuelReqSum = calcFuelReq modules fuelReq
    printfn "Sum of fuel requirements: %A" fuelReqSum

    let totalFuelReqSum = calcFuelReq modules totalFuelReq
    printfn "Sum of total fuel requirements: %A" totalFuelReqSum

    0
