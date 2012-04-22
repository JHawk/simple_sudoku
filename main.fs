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
        puzzle.Split(',') |> Array.map (fun row -> row |> Seq.map (fun c -> cell c) |> Seq.toArray)

module internal Solution =
    let string_p (p: Puzzle) : string =
        p |> Array.fold (fun acc a -> Array.concat [acc; a]) [||] |> Array.fold (fun s i -> s + i.ToString()) ""

    let lines (s: string) =
        let l = Cell.PossibleValues.Length
        seq { for i in l..(l+1)..s.Length -> i } |> Seq.fold (fun (s:string) idx -> s.Insert(idx,"\n")) s
        
    let pp (p: Puzzle) = printf "\n\npuzzle ::\n%s" <| (p |> string_p |> lines)

    let _check (a: Cell array) (cell: Cell): Cell =
        match cell.value with
            | None   -> let a = a |> Array.choose (fun c -> c.value)
                        let pv = cell.possible_values |> Array.filter (fun p -> a |> Array.forall (fun i -> p <> i))
                        let v = match pv.Length with 1 -> Some pv.[0] | _ -> None
                        { possible_values = pv; value = v }
            | Some v -> cell

    let check (puzzle: Puzzle) ri ci (cell: Cell) = 
        let row  = puzzle.[ri]
        let col  = puzzle |> Array.map (fun s -> s.[ci])
        let c_reg, r_reg = (ci/3) * 3, (ri/3) * 3
        let region = [| for r in (0 + r_reg)..(2 + r_reg) do 
                          for c in (0 + c_reg)..(2 + c_reg) do 
                              yield puzzle |> Seq.nth r |> Seq.nth c |]
        cell |> _check row |> _check col |> _check region

    let left (puzzle: Puzzle) = puzzle |> Array.sumBy (fun row -> row |> Array.sumBy (fun cell -> match cell.value with None -> 1 | _ -> 0))

    let solve (puzzle: Puzzle) =
        let rec solve' puzzle remaining count =
            printf "\n\nattempt %i\n" count
            printf "remaining :: %A\n" remaining
            pp puzzle
            if remaining < 1 then puzzle
            else let p = puzzle |> Array.mapi (fun ri row -> row |> Array.mapi (fun ci cell -> check puzzle ri ci cell))
                 let _remaining = left p
                 if remaining = _remaining then failwith "last iter :: no change"
                 solve' p _remaining (count + 1)
        solve' puzzle (Cell.PossibleValues.Length * Cell.PossibleValues.Length) 0

open Solution
open Setup

let solution = solve (format_puzzle puzzle)
printf <| if answer = string_p solution then "\n\n\n - THE END - \n\n\n" else "\n\n\n - DIDN'T MATCH - \n\n\n"
let a = Console.ReadLine()