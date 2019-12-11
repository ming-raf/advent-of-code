namespace AdventOfCode.Days.Day2

    module Day =
        open System
        open AdventOfCode.Days.DataStructure

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

        let intCode input =
            let rec evaluate input prev skipTimes =
                match input with
                | [] -> []
                | head::tail when head = 1 || head = 2 -> let first = (prev @ input).[tail.[0]]
                                                          let second = (prev @ input).[tail.[1]]
                                                          let newValue = if head = 1 then (first + second) else (first * second)
                                                          let outResult = replaceValue (prev @ input) [] newValue tail.[2] 0;
                                                          let newTail = skipTo outResult skipTimes 0;
                                                          let skippedVals = skipped outResult [] skipTimes 0;
                                                          if List.isEmpty newTail then outResult else evaluate newTail skippedVals (skipTimes + 4)
                | head::tail when head = 99 -> prev @ input
                | _ -> input
            evaluate input [] 3

        let findNounVerb input =
            let rec getPossibleValues input expected noun verb : list<int> =
                let x = replaceValue input [] noun 1 0;
                let y = replaceValue x [] verb 2 0;
                let z = intCode y
                match z with
                | [] -> []
                | z when z.[0] = expected -> [noun; verb]
                | z when noun = 99 && verb = 99 -> []
                | z when verb = 99 -> getPossibleValues input expected (noun + 1) 0
                | z when noun = 99 -> getPossibleValues input expected 0 (verb + 1)
                | _ -> getPossibleValues input expected noun (verb + 1)
            let ret = (getPossibleValues input 19690720 0 0)
            100 * ret.[0] + ret.[1]

        type Day2(filePath) =
            inherit AdventDayBase(filePath)
                member this.PartOneInput =
                    let input = this.ReadCommaSeparated
                    let newInput1 = replaceValue input [] 12 1 0
                    replaceValue newInput1 [] 2 2 0

                member this.PartTwoInput = this.ReadCommaSeparated

            interface IAdventDay with
                member this.PrintResults = printfn "IntCode: %d" (intCode(this.PartOneInput)).[0];
                                           printfn "NounVerb: %d" (findNounVerb(this.PartTwoInput));