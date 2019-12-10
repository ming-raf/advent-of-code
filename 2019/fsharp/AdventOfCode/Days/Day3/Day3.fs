namespace AdventOfCode.Days.Day3

    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        let rec moveRight x y steps currSteps : int * int =
            if currSteps < steps then moveRight (x+1) y steps (currSteps+1) else (x, y)

        let rec moveLeft x y steps currSteps : int * int =
            if currSteps < steps then moveLeft (x-1) y steps (currSteps+1) else (x, y)

        let rec moveUp x y steps currSteps : int * int =
            if currSteps < steps then moveUp x (y+1) steps (currSteps+1) else (x, y)

        let rec moveDown x y steps currSteps : int * int =
            if currSteps < steps then moveDown x (y-1) steps (currSteps+1) else (x, y)

        let getCoords input =
            let rec move (input: list<string>) x y (acc: list<int * int>) =
                match input with
                | [] -> acc
                | head::tail when head.[0] = 'R' -> let (newX, newY) = moveRight x y (int(head.[1..])) 0;
                                                    move tail newX newY ((newX, newY)::acc);
                | head::tail when head.[0] = 'L' -> let (newX, newY) = moveLeft x y (int(head.[1..])) 0;
                                                    move tail newX newY ((newX, newY)::acc);
                | head::tail when head.[0] = 'U' -> let (newX, newY) = moveUp x y (int(head.[1..])) 0;
                                                    move tail newX newY ((newX, newY)::acc);
                | head::tail when head.[0] = 'D' -> let (newX, newY) = moveDown x y (int(head.[1..])) 0;
                                                    move tail newX newY ((newX, newY)::acc);
                | _ -> acc
            let result = List.rev(move input 0 0 [(0, 0)])
            printfn "%A" result
            result

        let getLine (x1, y1) (x2, y2) =
            let a = y1 - y2
            let b = x2 - x1
            let c = (x1 * y2) - (x2 * y1)
            (a, b, -c)

        exception NoIntersectionException of string
        let intersection (a1, b1, c1) (a2, b2, c2) =
            let det = (a1 * b2) - (b1 * a2)
            let detX = (c1 * b2) - (b1 * c2)
            let detY = (a1 * c2) - (c1 * a2)

            if det <> 0 then
                let x = detX / det
                let y = detY / det

                (x, y)
            else raise (NoIntersectionException("Cannot find any intersection"))

        let doShit input1 input2 =
            let coords1 = getCoords input1
            let coords2 = getCoords input2

            let rec outer (i: int) (line1: int * int * int) (xs1: int) (ys1: int) (xd1: int) (yd1: int): int =
                let rec inner (j: int) (line2: int * int * int) (d: int): int =
                    match coords2 with
                    | [] -> d
                    | head::tail when j <= (coords2.Length-2) -> let (xs2, ys2) = coords2.[j]
                                                                 let (xd2, yd2) = coords2.[j+1]
                                                                 let line2 = getLine (xs2, ys2) (xd2, yd2)
                                                                 try
                                                                    let (intersectX, intersectY) = intersection line1 line2
                                                                    let minX1 = Math.Min(xs1, xd1)
                                                                    let maxX1 = Math.Max(xs1, xd1)
                                                                    let minX2 = Math.Min(xs2, xd2)
                                                                    let maxX2 = Math.Max(xs2, xd2)
                                                                    let retX = if (minX1 <= intersectX && intersectX <= maxX1) &&
                                                                                  (minX2 <= intersectX && intersectX <= maxX2) then intersectX else 9999

                                                                    let minY1 = Math.Min(ys1, yd1)
                                                                    let maxY1 = Math.Max(ys1, yd1)
                                                                    let minY2 = Math.Min(ys2, yd2)
                                                                    let maxY2 = Math.Max(ys2, yd2)
                                                                    let retY = if (minY1 <= intersectY && intersectY <= maxY1) &&
                                                                                  (minY2 <= intersectY && intersectY <= maxY2) then intersectY else 9999

                                                                    let dist = Math.Abs(retX) + Math.Abs(retY)
                                                                    printfn "(%d, %d) - (%d, %d) - dist=%d" retX retY intersectX intersectY dist
                                                                    inner (j+1) line2 dist
                                                                 with
                                                                    | :? NoIntersectionException -> 0;
                    | head::tail when d = 0 -> inner (j+1) line2 d
                    | _ -> d
                match coords1 with
                | [] -> inner 0 (0,0,0) 0
                | head::tail when i < (coords1.Length-2) -> let (xs1, ys1) = coords1.[i]
                                                            let (xd1, yd1) = coords1.[i+1]
                                                            outer (i+1) (getLine (xs1, ys1) (xd1, yd1)) xs1 ys1 xd1 yd1
                | _ -> inner 0 (0,0,0) 0
            outer 0 (0,0,0) 0 0 0 0
            // for i in 0 .. (coords1.Length - 2) do
            //     for j in 0 .. (coords2.Length - 2) do
            //         let (xs1, ys1) = coords1.[i]
            //         let (xd1, yd1) = coords1.[i+1]
            //         let line1 = getLine (xs1, ys1) (xd1, yd1)

            //         let (xs2, ys2) = coords2.[j]
            //         let (xd2, yd2) = coords2.[j+1]
            //         let line2 = getLine (xs2, ys2) (xd2, yd2)
            //         try
            //             let (intersectX, intersectY) = intersection line1 line2
            //             let minX1 = Math.Min(xs1, xd1)
            //             let maxX1 = Math.Max(xs1, xd1)
            //             let minX2 = Math.Min(xs2, xd2)
            //             let maxX2 = Math.Max(xs2, xd2)
            //             let retX = if (minX1 <= intersectX && intersectX <= maxX1) &&
            //                           (minX2 <= intersectX && intersectX <= maxX2) then intersectX else 9999

            //             let minY1 = Math.Min(ys1, yd1)
            //             let maxY1 = Math.Max(ys1, yd1)
            //             let minY2 = Math.Min(ys2, yd2)
            //             let maxY2 = Math.Max(ys2, yd2)
            //             let retY = if (minY1 <= intersectY && intersectY <= maxY1) &&
            //                           (minY2 <= intersectY && intersectY <= maxY2) then intersectY else 9999

            //             let dist = Math.Abs(retX) + Math.Abs(retY)
            //             printfn "(%d, %d) - (%d, %d) - dist=%d" retX retY intersectX intersectY dist
            //             ()
            //         with
            //             | :? NoIntersectionException -> ();

        type Day3(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults = let d = doShit ["R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72";] ["U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83";];
                                           printfn "%d" d
                                           //doShit ["R8"; "U5"; "L5"; "D3"] ["U7";"R6";"D4";"L4";]
