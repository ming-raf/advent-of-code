namespace AdventOfCode.Days

    module DataStructure =
        open System.IO

        type IAdventDay =
            abstract PrintResults : unit

        [<AbstractClass>]
        type AdventDayBase(filePath: string) =

            member this.ReadInputLines() = seq<int> {
                for line in File.ReadLines filePath do
                    yield int line }

            member this.ReadInputText() = File.ReadAllText(filePath)


