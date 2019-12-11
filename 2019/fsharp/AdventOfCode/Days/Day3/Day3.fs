namespace AdventOfCode.Days.Day3

    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        exception NoIntersectionException of string

        [<AllowNullLiteral>]
        type Point(x, y) =
            member this.X: int = x
            member this.Y: int =  y

        type Line(point1: Point, point2: Point) =
            member this.Point1: Point = point1
            member this.Point2: Point = point2

            member this.A: int = point1.Y - point2.Y
            member this.B: int = point2.X - point1.X
            member this.C: int = -((point1.X * point2.Y) - (point2.X * point1.Y))

        type Segment(line1: Line, line2: Line) =
            member this.Line1: Line = line1
            member this.Line2: Line = line2

            member this.CalculateLineIntersection: Point =
                let det = (line1.A * line2.B) - (line1.B * line2.A)
                let detX = (line1.C * line2.B) - (line1.B * line2.C)
                let detY = (line1.A * line2.C) - (line1.C * line2.A)

                if det <> 0 then
                    let x = detX / det
                    let y = detY / det

                    Point(x, y)
                else raise (NoIntersectionException("Cannot find any intersection"))

            member this.CalculateSegmentIntersection: Point =
                try
                    let intersection = this.CalculateLineIntersection
                    let minX1 = Math.Min(line1.Point1.X, line1.Point2.X)
                    let maxX1 = Math.Max(line1.Point1.X, line1.Point2.X)
                    let minX2 = Math.Min(line2.Point1.X, line2.Point2.X)
                    let maxX2 = Math.Max(line2.Point1.X, line2.Point2.X)
                    let retX = if (minX1 <= intersection.X && intersection.X <= maxX1) &&
                                  (minX2 <= intersection.X && intersection.X <= maxX2) then
                                  intersection.X else
                                  raise (NoIntersectionException("Intersection not in line segment"))

                    let minY1 = Math.Min(line1.Point1.Y, line1.Point2.Y)
                    let maxY1 = Math.Max(line1.Point1.Y, line1.Point2.Y)
                    let minY2 = Math.Min(line2.Point1.Y, line2.Point2.Y)
                    let maxY2 = Math.Max(line2.Point1.Y, line2.Point2.Y)
                    let retY = if (minY1 <= intersection.Y && intersection.Y <= maxY1) &&
                                  (minY2 <= intersection.Y && intersection.Y <= maxY2) then
                                  intersection.Y else
                                  raise (NoIntersectionException("Intersection not in line segment"))

                    Point(retX, retY)
                with
                    | :? NoIntersectionException -> null;

        let rec moveRight (prevPoint: Point) (steps: int) (currSteps: int) : Point =
            if currSteps < steps then moveRight (Point(prevPoint.X + 1, prevPoint.Y)) steps (currSteps+1) else prevPoint

        let rec moveLeft (prevPoint: Point) (steps: int) (currSteps: int) : Point =
            if currSteps < steps then moveLeft (Point(prevPoint.X - 1, prevPoint.Y)) steps (currSteps+1) else prevPoint

        let rec moveUp (prevPoint: Point) (steps: int) (currSteps: int) : Point =
            if currSteps < steps then moveUp (Point(prevPoint.X, prevPoint.Y + 1)) steps (currSteps+1) else prevPoint

        let rec moveDown (prevPoint: Point) (steps: int) (currSteps: int) : Point =
            if currSteps < steps then moveDown (Point(prevPoint.X, prevPoint.Y - 1)) steps (currSteps+1) else prevPoint

        let traverse (input: list<string>) =
            let rec startMoving (input: list<string>) (point: Point) (acc: list<Point>) =
                match input with
                | [] -> acc
                | head::tail when head.[0] = 'R' -> let newPoint = moveRight point (int(head.[1..])) 0;
                                                    startMoving tail newPoint (newPoint::acc);
                | head::tail when head.[0] = 'L' -> let newPoint = moveLeft point (int(head.[1..])) 0;
                                                    startMoving tail newPoint (newPoint::acc);
                | head::tail when head.[0] = 'U' -> let newPoint = moveUp point (int(head.[1..])) 0;
                                                    startMoving tail newPoint (newPoint::acc);
                | head::tail when head.[0] = 'D' -> let newPoint = moveDown point (int(head.[1..])) 0;
                                                    startMoving tail newPoint (newPoint::acc);
                | _ -> acc
            let result = List.rev(startMoving input (Point(0, 0)) [Point(0, 0)])
            //printfn "%A" result
            result

        let doShit input1 input2 =
            let t1 = traverse input1
            let t2 = traverse input2
            let intersections = seq {for i in 0 .. (t1.Length - 2) do
                                        for j in 0 .. (t2.Length - 2) do
                                            let line1 = Line(t1.[i], t1.[i + 1]);
                                            let line2 = Line(t2.[j], t2.[j + 1]);
                                            let segment = Segment(line1, line2)
                                            let intersection = segment.CalculateSegmentIntersection;
                                            yield intersection }
            let minDistance = intersections
                              |> Seq.filter (isNull >> not)
                              |> Seq.filter (fun point -> point.X <> 0 && point.Y <> 0)
                              |> Seq.map (fun point -> Math.Abs(point.X) + Math.Abs(point.Y))
                              |> Seq.min
            minDistance

        type Day3(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults = let lines = this.ReadCommaSeparatedLines |> Seq.toList;
                                           let shit = doShit (lines.[0]) (lines.[1])
                                           printfn "Closest Central Port Distance: %d" shit
