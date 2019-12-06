namespace AdventOfCode
    module Day =
        open System
        open System.IO
        open AdventDay

        let calculateFuel (mass : int) : int =
            int(Math.Floor(float(mass / 3))) - 2

        let readInput filePath = seq<int> {
            for line in File.ReadLines filePath do
                yield int line }

        type Day1() =
            interface IAdventDay with
                member this.Result =
                    readInput("input.txt")
                    |> Seq.sumBy(calculateFuel)