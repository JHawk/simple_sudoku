open System

type Cell = { possible_values: int array; value: int option }
            with override this.ToString() = match this.value with Some i -> i.ToString() | _ -> "." 
                 static member PossibleValues = [|1..9|]

type Puzzle = array<array<Cell>>

module internal Setup = 
    let puzzle = 
        "400050200,700100040,809604010,006273094,070000050,940815300,030901408,080002001,004030006"

    let answer = 
        "461357289723189645859624713516273894378496152942815367235961478687542931194738526"

    let format_puzzle (puzzle: string) : Puzzle = 
        let cell c = match Int32.Parse(c.ToString()) with 
                         | 0 -> { possible_values = Cell.PossibleValues ; value = None }
                         | i -> { possible_values = [|i|]               ; value = Some i }
        puzzle.Split(',') |> Array.map (fun row -> row |> Seq.map (fun c -> cell c) |>
