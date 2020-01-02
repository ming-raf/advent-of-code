namespace AdventOfCode.Days.Day7

    module PartOne =
        open System
        open AdventOfCode.Days.DataStructure

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

        let doStuff (input: List<int>) (opDef: int) (paramDefs: int[]) (diagnosticInput: int) (currentPointer: int): List<int> * int * int =
            let opDefLength = countDigits opDef
            let op = getDigit opDef 1 + ((getDigit opDef 2) * 10)
            let p1Mode = getDigit opDef 3
            let p2Mode = getDigit opDef 4
            let p3Mode = getDigit opDef 5

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
                (newInput, 3, 0)
            | 2 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let outValue = paramDefs.[2]
                let newInput = replaceValue input outValue (fst parameterValues * snd parameterValues)
                (newInput, 3, 0)
            | 3 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (paramDefs.[0], 0)
                                      | (_, _) -> (0, 0)
                let newInput = replaceValue input (fst parameterValues) diagnosticInput
                (newInput, 1, 0)
            | 4 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], 0)
                                      | (1, 0) -> (paramDefs.[0], 0)
                                      | (_, _) -> (0, 0)
                (input, 1, fst parameterValues)
            | 5 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let (p1, p2) = parameterValues
                if p1 <> 0 then
                    (input, p2 - currentPointer - 1, 0)
                else
                    (input, 2, 0)
            | 6 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let (p1, p2) = parameterValues
                if p1 = 0 then
                    (input, p2 - currentPointer - 1, 0)
                else
                    (input, 2, 0)
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
                    (newInput, 3, 0)
                else
                    let newInput = replaceValue input outValue 0
                    (newInput, 3, 0)
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
                    (newInput, 3, 0)
                else
                    let newInput = replaceValue input outValue 0
                    (newInput, 3, 0)
            | _ -> (input, 0, 0)

        let intCode (input: List<int>) (diagnosticInputs: List<int>) =

            let rec runInstructions (input: List<int>) (diagnosticInputs: List<int>) (count: int) (pointer: int) (acc: List<int>) (lastOutput: int) =
                match input with
                | [] -> (acc, lastOutput)
                | head::tail when count <> 0 -> runInstructions tail diagnosticInputs (count - 1) (pointer + 1) acc lastOutput
                | head::tail -> let op = tailDigits head 2
                                let opDefSize = countDigits head

                                if opDefSize = 2 && op = 99 then
                                    (acc, lastOutput)
                                else
                                    let paramDefs = if op = 1 || op = 2 || op = 7 || op = 8 then
                                                        [| acc.[pointer + 1]; acc.[pointer + 2]; acc.[pointer + 3]; |]
                                                    elif op = 3 || op = 4 then
                                                        [| acc.[pointer + 1]; |]
                                                    elif op = 5 || op = 6 then
                                                        [| acc.[pointer + 1]; acc.[pointer + 2]; |]
                                                    else [||]

                                    let (diagnosticInput, newDiagnosticInputs) = if op = 3 && diagnosticInputs.Length > 1 then
                                                                                    (diagnosticInputs.[0], diagnosticInputs.[1..])
                                                                                 else
                                                                                    (diagnosticInputs.[0], diagnosticInputs)

                                    let (newInput, jumpCount, output) = doStuff acc head paramDefs diagnosticInput pointer
                                    printfn "%A, %d %d input: %d" newInput jumpCount output diagnosticInput
                                    let (newTail, newPrev) = skipTo newInput pointer

                                    runInstructions newTail newDiagnosticInputs (jumpCount) (pointer + 1) newInput output

            runInstructions input diagnosticInputs 0  0 input 0

        let rec shiftRight (input: List<string>) (currentShift: int) (maxShift: int) =
            match input with
            | [] -> []
            | head::tail when currentShift = maxShift -> input
            | head::tail -> shiftRight ((input.[input.Length-1])::input.[..(input.Length-2)]) (currentShift + 1) maxShift

        // let rec getUniqueCombinations (input: List<string>) (size: int) (acc: List<string>): List<string> =
        //     let rec combine (input: string) (others: List<string>) (nextOthers: List<string>) (size: int) (acc: List<string>): List<string> =
        //         match others with
        //         | [] -> combine acc nextOthers (shiftRight nextOthers 0 1) size acc
        //         | head::tail when (acc |> List.filter (fun x -> x.Length = size)).Length > 0 -> acc
        //         | head::tail -> combine input tail nextOthers size ((input + head)::acc)

        //     match input with
        //     | [] -> acc
        //     | head::tail when head.Length = size -> getUniqueCombinations tail size (head::acc)
        //     | head::tail -> let combinedValues = combine head tail (shiftRight tail 0 1) size []
        //                     let combination = getUniqueCombinations combinedValues size acc
        //                     getUniqueCombinations tail size (combination @ acc)

        let listUniqueCombinations (input: List<'T>) =
            let rec combine (target: List<'T>) (others: List<'T>) (acc: List<List<'T>>): List<List<'T>> =
                match others with
                | [] -> acc
                | head::tail -> combine target tail ((target @ [head])::acc)

            let rec doStuff (combinations: List<List<'T>>) (acc: List<List<'T>>): List<List<'T>> =
                match combinations with
                | [] -> acc
                | head::tail when head.Length = input.Length -> doStuff tail (head::acc)
                | head::tail -> let missingValues = input |> List.filter (fun i -> not (head |> List.contains i))
                                let combinedValues = combine head missingValues []
                                let newCombinations = doStuff combinedValues []

                                // printfn "%A" newCombinations
                                doStuff tail (newCombinations @ acc)

            doStuff (input |> List.map (fun x -> [x;])) []

        let rec amplify (input: List<int>) (phaseSettings: List<int>) (prevOutput: int) (acc) =
            let firstPhaseSetting = if phaseSettings.Length > 0 then phaseSettings.[0] else -1
            printfn "%A Phase: %d PrevOutput: %d" input firstPhaseSetting prevOutput
            match phaseSettings with
            | [] -> acc
            | head::tail -> let (lastInput, lastOutput) = intCode input (head::[prevOutput])
                            amplify input tail lastOutput (acc @ [lastOutput])

        let start (input: List<int>) (combinations: List<int>) =
            let uniqueCombinations = listUniqueCombinations combinations 
                                     |> List.map (fun innerList -> innerList
                                                                   |> List.map char
                                                                   |> List.fold (fun a -> a::[]))

            printfn "Total combinations: %d" uniqueCombinations.Length

            let ret = uniqueCombinations |> List.map (fun c -> amplify c)

            uniqueCombinations

        type EntryPoint(filePath) =
            inherit AdventDayBase(filePath)

            member this.ReadInput = this.ReadCommaSeparated int

            interface IAdventDay with
                member this.PrintResults = printfn "%A" (start this.ReadInput [0;1;2;3;4;])