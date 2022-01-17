// All possible words
let readLines filePath = System.IO.File.ReadLines(filePath)

let isLength len (word:string) = word.Length = len
let ofLength len words:seq<string> =
    words
    |> Seq.filter (isLength len)

// Exclude grey letters
let intersect a b = Set.intersect (Set.ofList a) (Set.ofList b)
let hasIntersection a b = intersect a b |> Set.isEmpty

let hasLetters (letters:list<char>) (word:string) =
    Seq.toList word
    |> hasIntersection letters
let excludingLetters (letters:list<char>) (words:seq<string>) =
    words
    |> Seq.filter (hasLetters letters)

// Filter by green letters
type LetterPos = {Letter:char; Position:int}

let hasLetterPos (word:string) (lp:LetterPos) = word[lp.Position] = lp.Letter
let hasLetterPoses (lps:list<LetterPos>) (word:string) =
    lps
    |> List.forall (hasLetterPos word)
let withCorrectLetters (lps:list<LetterPos>) (words:seq<string>) =
    words
    |> Seq.filter (hasLetterPoses lps)

// Filter by yellow letters
let hasLetter letter word =
    Seq.toList word
    |> List.contains letter
let hasLetterNoPos (word:string) (lp:LetterPos) =
    (hasLetter lp.Letter word) && not (hasLetterPos word lp)
let hasLetterNoPoses (lps:list<LetterPos>) (word:string) =
    lps
    |> List.forall (hasLetterNoPos word)
let withMisplacedLetters (lps:list<LetterPos>) (words:seq<string>) =
    words
    |> Seq.filter (hasLetterNoPoses lps)

// Main
let length = 5
let excluded = ['r';'s';'t';'l';'n';'e';'z';'x';'w']
let correct = [
    { Letter = 'b'; Position = 0 };
    { Letter = 'y'; Position = 4 };
]
let misplaced = [
    { Letter = 'u'; Position = 3 }
]
let words =
    readLines "words_alpha.txt"
    |> ofLength length
    |> excludingLetters excluded
    |> withCorrectLetters correct
    |> withMisplacedLetters misplaced

words |> Seq.iter(fun x -> printfn "%s" x)
