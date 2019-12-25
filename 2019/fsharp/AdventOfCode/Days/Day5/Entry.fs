namespace AdventOfCode.Days.Day5

    module Entry =
        open System
        open Model
        open AdventOfCode.Days.DataStructure

        let intCode2 (input: List<int>) =
            let parsedInput = input |> List.map (fun x -> Param(x))

            let rec createInstructions (curr: List<Param>) (skipCount: int) (acc: List<Param>) =
                match curr with
                | [] -> []
                | head::tail when skipCount <> 0 -> createInstructions tail (skipCount - 1) acc
                | head::tail when Input(head.Value).OpCode = OpCode.Addition ||
                                  Input(head.Value).OpCode = OpCode.Multiplication ->
                                  let newInstruction = (Instruction(Input(head.Value), tail.[0], tail.[1], tail.[2]))
                                  let eval = newInstruction.Evaluate acc
                                  createInstructions tail (skipCount + 3) eval
                | head::tail when Input(head.Value).OpCode = OpCode.Assign ||
                                  Input(head.Value).OpCode = OpCode.Output ->
                                  let newInstruction = (Instruction(Input(head.Value), tail.[0]))
                                  let eval = newInstruction.Evaluate acc
                                  createInstructions tail (skipCount + 1) eval
                | head::tail when Input(head.Value).OpCode = OpCode.Stop ->
                                  acc
                | _ -> acc
            let instructions = createInstructions parsedInput 0 parsedInput
            printfn "%A" instructions

        type Day5(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults = printfn "IntCode: %A" (intCode2(this.ReadCommaSeparated));
