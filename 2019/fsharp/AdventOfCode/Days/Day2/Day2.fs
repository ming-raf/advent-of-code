namespace AdventOfCode.Days.Day2

    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        let batchesOf size input =
          // Inner function that does the actual work.
          // 'input' is the remaining part of the list, 'num' is the number of elements
          // in a current batch, which is stored in 'batch'. Finally, 'acc' is a list of
          // batches (in a reverse order)
          let rec loop input num batch acc =
            match input with
            | [] ->
                // We've reached the end - add current batch to the list of all
                // batches if it is not empty and return batch (in the right order)
                if batch <> [] then (List.rev batch)::acc else acc
                |> List.rev
            | x::xs when num = size - 1 ->
                // We've reached the end of the batch - add the last element
                // and add batch to the list of batches.
                loop xs 0 [] ((List.rev (x::batch))::acc)
            | x::xs ->
                // Take one element from the input and add it to the current batch
                loop xs (num + 1) (x::batch) acc
          loop input 0 [] []

        let DoStuff input =
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

            let rec skip (lst: list<int>) (steps: int) (currSteps: int) (skipped: list<int>) =
                match lst with
                    | [] -> []
                    | head::tail when currSteps < steps -> skip tail steps (currSteps + 1) (head::skipped)
                    | _ -> lst

            let rec _doStuff input prev ret =
                match input with
                | head::tail when head = 1 -> let op = getOp tail [] 0;
                                              printfn "Add: %A" op;
                                              let s = skip tail 3 0;
                                              printfn "Skip: %A" s.[1];
                                              let p = head::(s.[1] @ prev)
                                              printfn "Prev: %A" p;
                                              let r = replaceValue (prev @ input) [] (op.[0] + op.[1]) op.[2] 0;
                                              printfn "Result: %A" r;
                                              _doStuff s (head::prev) r
                | head::tail when head = 2 -> let op = getOp tail [] 0;
                                              printfn "Mult: %A" op;
                                              let s = skip tail 3 0;
                                              printfn "Skip: %A" s;
                                              let p = head::(s @ prev)
                                              printfn "Prev: %A" p;
                                              let r = replaceValue (prev @ input) [] (op.[0] * op.[1]) op.[2] 0;
                                              printfn "Result: %A" r;
                                              _doStuff (skip tail 3 0) (head::prev) r
                // | head::tail when head <> 1 || head <> 2 -> _doStuff tail
                | _ -> ret
            _doStuff input

        type Day2(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults = DoStuff([ 1; 10; 20; 30; 2; 15; 25; 35; ])