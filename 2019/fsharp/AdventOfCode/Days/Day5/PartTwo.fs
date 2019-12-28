namespace AdventOfCode.Days.Day5

    module PartTwo =
        open System
        open AdventOfCode.Days.DataStructure

        let mutable lastOutput = 0
        // let logFilePath = """C:\LocalRepo\advent-of-code\2019\fsharp\debug.log"""

        let countDigits number = (int) (log10 ((float)number)) + 1;

        // Right to left starting from 1
        let getDigit number (ind: int) = (number / int(Math.Pow(10.0, float(ind) - 1.0))) % 10;

        let tailDigits number n: int =

            let rec mult c acc =
                if c < n then
                    mult (c + 1) (acc * 10)
                else acc

            let m = mult 0 1
            let d = int(float(number / m)) * m
            (number - d)

        let replaceValue (input: list<int>) (replIndex: int) (value: int) =
            let rec _replaceValue (list: list<int>) (acc: list<int>) (value: int) (replIndex: int) (currIndex: int) : list<int> =
                match list with
                | [] -> []
                | head::tail when currIndex = replIndex -> acc @ [value] @ tail;
                | head::tail -> _replaceValue tail (acc @ [head]) value replIndex (currIndex + 1)
            _replaceValue input [] value replIndex 0

        let skipTo (input: List<int>) (steps: int) =
            let rec _skipTo (input: List<int>) (steps: int) (currSteps: int) (prev: List<int>): List<int> * List<int> =
                match input with
                    | [] -> ([], prev)
                    | head::tail when currSteps <= steps -> _skipTo tail steps (currSteps + 1) (prev @ [head])
                    | _ -> (input, prev)
            _skipTo input steps 0 []

        let doStuff (input: List<int>) (opDef: int) (paramDefs: int[]) (diagnosticInput: int) (currentPointer: int): List<int> * int =
            let opDefLength = countDigits opDef
            let op = getDigit opDef 1 + ((getDigit opDef 2) * 10)
            let p1Mode = getDigit opDef 3
            let p2Mode = getDigit opDef 4
            let p3Mode = getDigit opDef 5

            // printfn "%d %d" opDef op

            match op with
            | 1 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let outValue = paramDefs.[2]
                let newInput = replaceValue input outValue (fst parameterValues + snd parameterValues)
                (newInput, 3)
            | 2 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let outValue = paramDefs.[2]
                let newInput = replaceValue input outValue (fst parameterValues * snd parameterValues)
                (newInput, 3)
            | 3 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (paramDefs.[0], 0)
                                      | (_, _) -> (0, 0)
                let newInput = replaceValue input (fst parameterValues) diagnosticInput
                (newInput, 1)
            | 4 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], 0)
                                      | (1, 0) -> (paramDefs.[0], 0)
                                      | (_, _) -> (0, 0)
                // printfn "OUTPUT: %d" (fst parameterValues)
                // System.IO.File.AppendAllText(logFilePath, "OUTPUT: " + string(fst parameterValues) + "\n")
                lastOutput <- fst parameterValues
                (input, 1)
            | 5 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let (p1, p2) = parameterValues
                if p1 <> 0 then
                    (input, p2 - currentPointer - 1)
                else
                    (input, 2)
            | 6 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let (p1, p2) = parameterValues
                if p1 = 0 then
                    (input, p2 - currentPointer - 1)
                else
                    (input, 2)
            | 7 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let (p1, p2) = parameterValues
                let outValue = paramDefs.[2]
                if p1 < p2 then
                    let newInput = replaceValue input outValue 1
                    (newInput, 3)
                else
                    let newInput = replaceValue input outValue 0
                    (newInput, 3)
            | 8 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let (p1, p2) = parameterValues

                let outValue = paramDefs.[2]
                if p1 = p2 then
                    let newInput = replaceValue input outValue 1
                    (newInput, 3)
                else
                    let newInput = replaceValue input outValue 0
                    (newInput, 3)
            | _ -> (input, 0)

        let start (input: List<int>) (diagnosticInput: int) =

            // System.IO.File.Delete logFilePath

            // printfn "ORIGINAL INPUT: %A" input
            // printfn "DIAGNOSTIC INPUT: %d" diagnosticInput

            let rec runInstructions (_input: List<int>) (_count: int) (_pointer: int) (_acc: List<int>) =
                match _input with
                | [] -> _acc
                | head::tail when _count <> 0 -> runInstructions tail (_count - 1) (_pointer + 1) _acc
                | head::tail -> let op = tailDigits head 2
                                let opDefSize = countDigits head

                                if opDefSize = 2 && op = 99 then
                                    _acc
                                else
                                    let paramDefs = if op = 1 || op = 2 || op = 7 || op = 8 then
                                                        [| _acc.[_pointer + 1]; _acc.[_pointer + 2]; _acc.[_pointer + 3]; |]
                                                    elif op = 3 || op = 4 then
                                                        [| _acc.[_pointer + 1]; |]
                                                    elif op = 5 || op = 6 then
                                                        [| _acc.[_pointer + 1]; _acc.[_pointer + 2]; |]
                                                    else [||]

                                    let (newInput, jumpCount) = doStuff _acc head paramDefs diagnosticInput _pointer

                                    // let dpoint = "POINTER: " + _pointer.ToString() + " OP: " + op.ToString() + " PARAMS: " + String.Join(", ", paramDefs)
                                    // let dnIn = String.Join(" ", newInput)
                                    // System.IO.File.AppendAllText(logFilePath, dpoint + "\n" + dnIn + "\n")
                                    // printfn "POINTER: %d OP: %d" _pointer op
                                    // printfn "%A" newInput
                                    let (newTail, newPrev) = skipTo newInput _pointer

                                    runInstructions newTail (jumpCount) (_pointer + 1) newInput

            let finalInput = runInstructions input 0  0 input

            printfn "FINAL OUTPUT: %d" lastOutput
            // printfn "%A" finalInput

        type EntryPoint(filePath) =
            inherit AdventDayBase(filePath)

            member this.ReadInput = this.ReadCommaSeparated int

            interface IAdventDay with
                member this.PrintResults = start this.ReadInput 5