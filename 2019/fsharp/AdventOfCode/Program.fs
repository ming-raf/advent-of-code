namespace AdventOfCode

module Main =

    open AdventOfCode.Days
    open AdventOfCode.Days.Day1

    [<EntryPoint>]
    let main argv =
        let day1 = Day.Day1("Days\Day1\input.txt")
        (day1 :> DataStructure.IAdventDay).PrintResults
        
        
        0