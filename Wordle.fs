module Wordle

type LetterPosition = { Letter: char; Position: int }

type BoardState =
    { Excluded: list<char>
      Correct: list<LetterPosition>
      Misplaced: list<LetterPosition> }

// Exclude grey letters
let intersect a b =
    Set.intersect (Set.ofList a) (Set.ofList b)

let noIntersection a b = intersect a b |> Set.isEmpty

let hasNoLetters (letters: list<char>) (word: string) =
    Seq.toList word |> noIntersection letters

let excludingLetters letters words =
    words |> Seq.filter (hasNoLetters letters)

// Filter by green letters
let hasLetterInPosition (word: string) (lp: LetterPosition) = word[lp.Position] = lp.Letter

let hasLettersInPositions lps word =
    lps |> List.forall (hasLetterInPosition word)

let withCorrectLetters lps words =
    words |> Seq.filter (hasLettersInPositions lps)

// Filter by yellow letters
let hasLetter letter word = Seq.toList word |> List.contains letter

let hasLetterOutOfPosition (word: string) (lp: LetterPosition) =
    (hasLetter lp.Letter word)
    && not (hasLetterInPosition word lp)

let hasLettersOutOfPositions lps word =
    lps |> List.forall (hasLetterOutOfPosition word)

let withMisplacedLetters lps words =
    words |> Seq.filter (hasLettersOutOfPositions lps)

let solve words state =
    words
    |> excludingLetters state.Excluded
    |> withCorrectLetters state.Correct
    |> withMisplacedLetters state.Misplaced
