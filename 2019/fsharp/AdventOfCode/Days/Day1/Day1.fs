namespace AdventOfCode.Days.Day1
    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        let calculateFuel (mass : int) : int =
            int(Math.Floor(float(mass / 3))) - 2

        type Day1(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults =
                    let totalFuel = this.ReadInput()
                                    |> Seq.sumBy(calculateFuel)
                    printfn "Total Fuel: %d" totalFuel
                    ()
