namespace AdventOfCode.Days.Day1

    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        let calculateFuel (mass : int) : int =
            int(Math.Floor(float(mass / 3))) - 2

        let calculateFuelofFuel (mass: int) : int =
            let rec total (mass: int) : int =
                match mass with
                | mass when mass < 0 -> 0
                | _ -> mass + total(calculateFuel(mass));
            total(mass) - mass

        type Day1(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults =
                    let totalFuel = this.ReadInputLines()
                                    |> Seq.sumBy(calculateFuel)
                    printfn "Total fuel: %d" totalFuel

                    let totalFuelofFuel = this.ReadInputLines()
                                          |> Seq.sumBy(calculateFuelofFuel)
                    printfn "Total fuel of fuel: %d" totalFuelofFuel
                    ()
