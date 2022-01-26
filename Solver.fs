module Solver

// Exclude grey chars
let intersect a b =
    Set.intersect (Set.ofList a) (Set.ofList b)

let noIntersection a b = intersect a b |> Set.isEmpty

let hasNoChars (chars: char list) (word: string) = Seq.toList word |> noIntersection chars

let excludingChars chars words = words |> Seq.filter (hasNoChars chars)

// Filter by green chars
let hasCharInPosition (word: string) (cp: CharPosition) = word[cp.Position] = cp.Char

let hasCharsInPositions cps word =
    cps |> List.forall (hasCharInPosition word)

let withCorrectChars cps words =
    words |> Seq.filter (hasCharsInPositions cps)

// Filter by yellow chars
let hasChar char word = Seq.toList word |> List.contains char

let hasCharOutOfPosition (word: string) (cp: CharPosition) =
    (hasChar cp.Char word)
    && not (hasCharInPosition word cp)

let hasCharsOutOfPositions cps word =
    cps |> List.forall (hasCharOutOfPosition word)

let withMisplacedChars cps words =
    words |> Seq.filter (hasCharsOutOfPositions cps)

let solve words state =
    words
    |> excludingChars state.Excluded
    |> withCorrectChars state.Correct
    |> withMisplacedChars state.Misplaced
