open System
open System.IO
open System.Collections.Generic

type SpaceObject = string

type CenterOfMass =
    | UniversalCenterOfMass
    | SpaceObject of SpaceObject

let getUniqueSpaceObjects (orbits: string list): Set<SpaceObject> =
    orbits
    |> List.map (fun o ->
        let parts = o.Split(')')
        seq { parts.[0]; parts.[1] })
    |> Seq.ofList
    |> Seq.concat
    |> Set.ofSeq

type OrbitMap = IDictionary<SpaceObject,CenterOfMass>

let getOrbitMap (orbits: string list): OrbitMap = 
    orbits
    |> List.map (fun o ->
        let parts = o.Split(')')
        let com =
            match parts.[0] with
            | "COM" -> UniversalCenterOfMass
            | x -> SpaceObject x
        let obj = parts.[1]
        (obj, com))
    |> dict

let rec countOrbits (map: OrbitMap) (origin: SpaceObject) =
    match map.[origin] with
    | UniversalCenterOfMass -> 1
    | SpaceObject obj -> (countOrbits map obj) + 1

let countTotalNumberOfOrbits (map: OrbitMap) (uniqueObjects: Set<SpaceObject>) =
    uniqueObjects
    |> Set.toList
    |> List.map (countOrbits map)
    |> List.sum

[<EntryPoint>]
let main argv =

    let input = File.ReadAllLines "Input.txt" |> List.ofArray

    let uniqueObjects = (getUniqueSpaceObjects input) |> Set.remove "COM"
    let map = getOrbitMap input

    let totalNumberOfOrbits = countTotalNumberOfOrbits map uniqueObjects

    printfn "Total number of orbits: %A" totalNumberOfOrbits

    0
