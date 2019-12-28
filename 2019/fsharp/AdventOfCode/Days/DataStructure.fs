namespace AdventOfCode.Days

    module DataStructure =
        open System.IO

        type IAdventDay =
            abstract PrintResults : unit

        [<AbstractClass>]
        type AdventDayBase(filePath: string) =

            member this.ReadText = File.ReadAllText filePath

            member this.ReadInputLines castType = File.ReadAllLines(filePath)
                                                  |> Array.map (castType) 
                                                  |> Array.toList

            member this.ReadCommaSeparated castType = (File.ReadAllText(filePath).Split ',')
                                                      |> Array.map (castType)
                                                      |> Array.toList

            member this.ReadCommaSeparatedLines = seq { for line in File.ReadLines filePath do
                                                        let items = line.Split ',' |> Array.toList
                                                        yield items }
