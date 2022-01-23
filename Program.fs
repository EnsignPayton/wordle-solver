open FSharp.Json
open Wordle

let isLength len (word: string) = word.Length = len

let ofLength len words = words |> Seq.filter (isLength len)

let wordList =
    System.IO.File.ReadLines "words_alpha.txt"
    |> ofLength 5

let stateJson = System.IO.File.ReadAllText "state.json"

let state = Json.deserialize<Wordle.BoardState> stateJson

let words = solve wordList state

words |> Seq.iter (fun x -> printfn "%s" x)
