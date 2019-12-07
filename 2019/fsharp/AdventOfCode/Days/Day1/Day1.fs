namespace AdventOfCode.Days.Day1
    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        let calculateFuel (mass : int) : int =
            int(Math.Floor(float(mass / 3))) - 2

        let rec _calculateFuelofFuel (mass: int) : int =
            match mass with
            | mass when mass < 0 -> 0
            | _ -> mass + _calculateFuelofFuel(calculateFuel(mass));

        let calculateFuelofFuel (mass: int) : int =
            _calculateFuelofFuel(mass) - mass

        type Day1(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults =
                    let totalFuel = this.ReadInput()
                                    |> Seq.sumBy(calculateFuel)
                    printfn "Total fuel: %d" totalFuel

                    let totalFuelofFuel = this.ReadInput()
                                          |> Seq.sumBy(calculateFuelofFuel)
                    printfn "Total fuel of fuel: %d" totalFuelofFuel
                    ()
