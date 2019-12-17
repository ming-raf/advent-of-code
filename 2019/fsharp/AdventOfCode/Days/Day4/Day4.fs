namespace AdventOfCode.Days.Day4

    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        // It is a six-digit number.
        // The value is within the range given in your puzzle input.
        // Two adjacent digits are the same (like 22 in 122345).
        // Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
        type PartOne(input: string) =

            let rec isValidDiffPasswords (digits: List<int>) (prev: int) (foundDouble: bool) =
                match digits with
                | [] -> foundDouble
                | head::tail when not foundDouble && prev = head -> isValidDiffPasswords (head::tail) head true
                | head::tail when prev <= head -> isValidDiffPasswords tail head foundDouble
                | head::tail -> false

            let rec countDiffPasswords (startRange: int) (endRange: int) (count: int) =
                if startRange > endRange then
                    count
                else
                    let digits: List<int> = string(startRange) |> List.ofSeq |> List.map (int)
                    if (isValidDiffPasswords digits -1 false) then
                        countDiffPasswords (startRange + 1) endRange (count + 1)
                    else
                        countDiffPasswords (startRange + 1) endRange count

            member this.TotalDiffPasswords =
                let tokens = input.Split '-' |> Array.toList
                let startRange = int(tokens.[0])
                let endRange = int(tokens.[1])

                countDiffPasswords startRange endRange 0

        // 112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
        // 123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
        // 111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
        type PartTwo(input: string) =

            let rec moveDuplicate (digits: List<int>) (prev: int) (acc: List<int>)=
                match digits with
                | [] -> acc
                | head::tail when head = prev -> head::acc
                | _ -> acc

            let rec isValidDiffPasswords (digits: List<int>) (prev: int) (count: int) (foundDouble: bool) =
                match digits with
                | [] -> foundDouble || count = 2
                | head::tail when prev = head -> isValidDiffPasswords tail head (count + 1) foundDouble
                | head::tail when prev < head -> if count = 2 then isValidDiffPasswords tail head 1 true else isValidDiffPasswords tail head 1 foundDouble
                | head::tail -> false

            let rec countDiffPasswords (startRange: int) (endRange: int) (count: int) =
                if startRange > endRange then
                    count
                else
                    let digits: List<int> = string(startRange) |> List.ofSeq |> List.map (int)
                    if (isValidDiffPasswords digits -1 1 false) then
                        countDiffPasswords (startRange + 1) endRange (count + 1)
                    else
                        countDiffPasswords (startRange + 1) endRange count

            member this.TotalDiffPasswords =
                let tokens = input.Split '-' |> Array.toList
                let startRange = int(tokens.[0])
                let endRange = int(tokens.[1])

                countDiffPasswords startRange endRange 0

        type Day4(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults =  let partOne = PartOne(this.ReadText)
                                            printfn "Total Different Passwords: %d" (partOne.TotalDiffPasswords)

                                            let partTwo = PartTwo(this.ReadText)
                                            printfn "Total Different Passwords 2: %d" (partTwo.TotalDiffPasswords)
