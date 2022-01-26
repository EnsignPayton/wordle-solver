[<AutoOpen>]
module Types

type CharPosition = { Char: char; Position: int }

type State =
    { Excluded: char list
      Correct: CharPosition list
      Misplaced: CharPosition list }
