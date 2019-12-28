namespace AdventOfCode.Days.Day6

    module PartOne =
        open System
        open AdventOfCode.Days.DataStructure

        [<AllowNullLiteral>]
        type Node =
            val Value: string
            val mutable Links: List<string>
            val mutable Prevs: List<string>

            new (value: string, links: List<string>) =
                { Value = value; Links = links; Prevs = []; }

            override this.ToString() = this.Value + ", (" + String.Join(", ", this.Links) + ")"

        let sumSearch (input: List<Node>) (startingNodeValue: string) (targetNodeValue: string) =

            let findNode (collection: List<Node>) value = let result = collection |> List.tryFind (fun n -> n.Value = value)
                                                          match result with
                                                          | Some n -> n
                                                          | None -> null

            let startNode = findNode input startingNodeValue
            let mutable visitedValues: List<string> = []

            let rec dfs (links: List<string>) (targetNodeValue: string) (acc: List<string>) =
                match links with
                | [] -> acc
                | head::tail when head = targetNodeValue -> (acc @ [head])
                | head::tail when visitedValues |> List.contains head -> dfs tail targetNodeValue acc
                | head::tail -> visitedValues <- visitedValues @ [head]
                                let node = findNode input head
                                let newAcc = if not (isNull node) then dfs node.Links targetNodeValue (acc @ [head]) else (acc @ [head])
                                // printfn "ACC: %A" newAcc
                                if newAcc.[newAcc.Length - 1] <> targetNodeValue then
                                    dfs tail targetNodeValue acc
                                else
                                    dfs tail targetNodeValue newAcc

            let matchedPath = dfs startNode.Links targetNodeValue []
            matchedPath.Length

        let start (input: List<string>) =

            let rec parse (input: List<string>) (acc: List<Node>) =
                match input with
                | [] -> acc
                | head::tail -> let tokens = head.Split ')'
                                let value = tokens.[0]
                                let link = tokens.[1]
                                let findExistingNode = acc |> List.tryFind (fun x -> x.Value = value)
                                let existingNode = match findExistingNode with
                                                   | Some x -> x
                                                   | None -> null

                                if not (isNull existingNode) then
                                    existingNode.Links <- (existingNode.Links @ [link])
                                    parse tail acc
                                else
                                    parse tail (acc @ [Node(value, [link])])
            let graph = parse input []
            printfn "%A" graph

            let allNodeValues = graph
                                |> List.collect (fun n -> [n.Value] @ n.Links)
                                |> List.distinct
                                |> List.filter (fun n -> n <> "COM")
            printfn "%A" allNodeValues

            let totalOrbits = allNodeValues |> List.sumBy (fun n -> sumSearch graph "COM" n)
            totalOrbits

        type EntryPoint(filePath) =
            inherit AdventDayBase(filePath)

            member this.ReadInput = this.ReadInputLines string

            interface IAdventDay with
                member this.PrintResults = printfn "%A" (start(this.ReadInput))