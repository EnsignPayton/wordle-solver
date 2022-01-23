open FSharp.Json
open Wordle

let readLines filePath = System.IO.File.ReadLines(filePath)

let readText filePath = System.IO.File.ReadAllText(filePath)

let isLength len (word: string) = word.Length = len

let ofLength len words = words |> Seq.filter (isLength len)

let wordList = readLines "words_alpha.txt" |> ofLength 5

let stateJson = readText "state.json"

let state = Json.deserialize<Wordle.BoardState> stateJson

let words = solve wordList state

words |> Seq.iter (fun x -> printfn "%s" x)
