namespace AdventOfCode
module Main =
    [<EntryPoint>]
    let main argv =
        let day1 = Day.Day1()
        printfn "%d" (day1 :> AdventDay.IAdventDay).Result
        0