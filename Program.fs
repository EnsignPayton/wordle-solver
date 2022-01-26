open Solver

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

        Seq.concat [ a; b ]
        |> Seq.map (fun x -> (x.Split('#')[0]).Trim())
        |> ofLength 4

// TODO: actually handle error cases
let args = System.Environment.GetCommandLineArgs()

let wordList =
    match args[1] with
    | "en" -> English.wordList
    | "jp" -> Japanese.wordList
    | _ -> []

let state = StateParser.parse args[2]

let words = solve wordList state

words |> Seq.iter (fun x -> printfn "%s" x)
