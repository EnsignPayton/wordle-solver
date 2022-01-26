module StateParser

open System.Text.RegularExpressions

let charToInt c = int c - int '0'

let pattern = "([a-zぁ-ん])+\\,([a-zぁ-ん]\\d)*\\,([a-zぁ-ん]\\d)*"

let parseChar (x: Capture) = x.Value[0]

let parseCharPos (x: Capture) =
    { Char = x.Value[0]
      Position = x.Value[1] |> charToInt }

let parse (s: string) =
    let rMatch = Regex.Match(s, pattern)

    let gray =
        rMatch.Groups[1].Captures
        |> Seq.map parseChar
        |> Seq.toList

    let green =
        rMatch.Groups[2].Captures
        |> Seq.map parseCharPos
        |> Seq.toList

    let yellow =
        rMatch.Groups[3].Captures
        |> Seq.map parseCharPos
        |> Seq.toList

    { Excluded = gray
      Correct = green
      Misplaced = yellow }
