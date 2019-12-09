namespace AdventOfCode.Days.Day2

    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        let rec getOp (lst: list<int>) (ret: list<int>) (current: int): list<int> =
            match lst with
            | [] -> List.rev ret
            | head::tail when Seq.contains current { 0 .. 1 }  -> getOp tail (head::ret) (current + 1)
            | head::tail when current = 2 -> List.rev(head::ret)
            | head::tail -> getOp tail ret (current + 1)

        let rec replaceValue (lst: list<int>) (acc: list<int>) (value: int) (replIndex: int) (currIndex: int) : list<int> =
            match lst with
            | [] -> []
            | head::tail when currIndex = replIndex -> acc @ [value] @ tail;
            | head::tail -> replaceValue tail (acc @ [head]) value replIndex (currIndex + 1)

        let rec skipTo (lst: list<int>) (steps: int) (currSteps: int) =
            match lst with
                | [] -> []
                | head::tail when currSteps <= steps -> skipTo tail steps (currSteps + 1)
                | _ -> lst

        let rec skipped (lst: list<int>) (acc: list<int>) (steps: int) (currSteps:int) =
            match lst with
                | [] -> []
                | head::tail when currSteps <= steps -> skipped tail (head::acc) steps (currSteps + 1)
                | _ -> List.rev acc

        // Feels like this is a shit way of solving this
        let intCode input =
            let rec evaluate input prev skipTimes =
                match input with
                | [] -> []
                | head::tail when head = 1 || head = 2 -> let op = getOp tail [] 0;
                                                          //printfn "Add: %A" op;
                                                          let first = (prev @ input).[op.[0]]
                                                          let second = (prev @ input).[op.[1]]
                                                          let newValue = if head = 1 then (first + second) else (first * second)
                                                          let outResult = replaceValue (prev @ input) [] newValue op.[2] 0;
                                                          //printfn "Result: %A" outResult;
                                                          let newTail = skipTo outResult skipTimes 0;
                                                          //printfn "Skip: %A" newTail;
                                                          let skippedVals = skipped outResult [] skipTimes 0;
                                                          //printfn "Skipped: %A" skippedVals;
                                                          if List.isEmpty newTail then outResult else evaluate newTail skippedVals (skipTimes + 4)
                | head::tail when head = 99 -> prev @ input
                | _ -> input
            evaluate input [] 3

        type Day2(filePath) =
            inherit AdventDayBase(filePath)
                member this.PartOneInput =
                    let input = this.ReadCommaSeparated()
                    let newInput1 = replaceValue input [] 12 1 0
                    replaceValue newInput1 [] 2 2 0

            interface IAdventDay with
                member this.PrintResults = printfn "IntCode: %d" (intCode(this.PartOneInput)).[0]