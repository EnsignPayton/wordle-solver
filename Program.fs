open Wordle

let readLines filePath =
    System.IO.File.ReadLines(filePath)

let isLength len (word:string) =
    word.Length = len

let ofLength len (words:seq<string>) =
    words
    |> Seq.filter (isLength len)

let wordList =
    readLines "words_alpha.txt"
    |> ofLength 5

let state = {
    Excluded = [
        'r';'s';'t';'l';'n';'e';'z';'x';'w'
    ];
    Correct = [
        {Letter='b'; Position=0};
        {Letter='y'; Position=4};
    ];
    Misplaced = [
        {Letter='u'; Position=3};
    ];
}

let words = solve wordList state

words |> Seq.iter(fun x -> printfn "%s" x)
