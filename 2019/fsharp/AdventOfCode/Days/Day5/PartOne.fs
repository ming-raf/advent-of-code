namespace AdventOfCode.Days.Day5

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

        let doStuff (input: List<int>) (opDef: int) (paramDefs: int[]): List<int> =
            let opDefLength = countDigits opDef
            let op = getDigit opDef 1 + ((getDigit opDef 2) * 10)
            let p1Mode = getDigit opDef 3
            let p2Mode = getDigit opDef 4
            let p3Mode = getDigit opDef 5

            printfn "%d %d" opDef op

            match op with
            | 1 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let outValue = paramDefs.[2]
                replaceValue input outValue (fst parameterValues + snd parameterValues)
            | 2 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], input.[paramDefs.[1]])
                                      | (0, 1) -> (input.[paramDefs.[0]], paramDefs.[1])
                                      | (1, 0) -> (paramDefs.[0], input.[paramDefs.[1]])
                                      | (1, 1) -> (paramDefs.[0], paramDefs.[1])
                                      | (_, _) -> (0, 0)
                let outValue = paramDefs.[2]
                replaceValue input outValue (fst parameterValues * snd parameterValues)
            | 3 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (paramDefs.[0], 0)
                                      | (_, _) -> (0, 0)
                replaceValue input (fst parameterValues) 1
            | 4 ->
                let parameterValues = match (p1Mode, p2Mode) with
                                      | (0, 0) -> (input.[paramDefs.[0]], 0)
                                      | (_, _) -> (0, 0)
                printfn "OUTPUT: %d" (fst parameterValues)
                input
            | _ -> input

        let start (input: List<int>) =

            let rec runInstructions (_input: List<int>) (_count: int) (_acc: List<int>) =
                match _input with
                | [] -> _acc
                | head::tail when _count <> 0 -> runInstructions tail (_count - 1) _acc
                | head::tail when let op = tailDigits head 2
                                  op = 1 || op = 2 -> let newInput = doStuff _acc head [| tail.[0]; tail.[1]; tail.[2]; |]
                                                      runInstructions tail (_count + 3) newInput
                | head::tail when let op = tailDigits head 2
                                  op = 3 || op = 4 -> let newInput = doStuff _acc head [| tail.[0]; |]
                                                      runInstructions tail (_count + 1) newInput
                | head::tail when let op = tailDigits head 2;
                                  op = 99 -> _acc
                | head::tail -> runInstructions tail 0 _acc
            let out = runInstructions input 0 input

            printfn "out: %A" out

        type EntryPoint(filePath) =
            inherit AdventDayBase(filePath)

            member this.ReadInput = this.ReadCommaSeparated int

            interface IAdventDay with
                member this.PrintResults = start this.ReadInput