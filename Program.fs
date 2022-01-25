open FSharp.Json
open Wordle

let isLength len (word: string) = word.Length = len

let ofLength len words = words |> Seq.filter (isLength len)

module English =
    let wordList =
        System.IO.File.ReadLines "words_alpha.txt"
        |> ofLength 5

module Japanese =
    let wordList =
        let a = System.IO.File.ReadLines "gcanna.t"
        let b = System.IO.File.ReadLines "gcannaf.t"
        Seq.concat [a; b]
        |> Seq.map (fun x -> (x.Split('#')[0]).Trim())
        |> ofLength 4

// TODO: Take state in a way that doesn't get checked in.
let state =
    "state.json"
    |> System.IO.File.ReadAllText
    |> Json.deserialize<Wordle.BoardState>

let words = solve Japanese.wordList state

words |> Seq.iter (fun x -> printfn "%s" x)
