namespace AdventOfCode.Days.Day3

    module Day =
        open System
        open AdventOfCode.Days.DataStructure

        exception NoIntersectionException of string

        [<AllowNullLiteral>]
        type Point(x, y) =
            member this.X: int = x
            member this.Y: int =  y

            override this.Equals obj =
                match obj with
                | :? Point as obj -> obj.X = this.X && obj.Y = this.Y
                | _ -> false

            override this.ToString() =
                "(" + string(this.X) + ", " + string(this.Y) + ")"

        // Use 'long' for potentially big calculation stuff because 
        // it f*cked my brain up for hours trying to figure out why this is failing
        type Line(point1: Point, point2: Point) =
            member this.Point1: Point = point1
            member this.Point2: Point = point2

            member this.A: int64 = int64(point1.Y) - int64(point2.Y)
            member this.B: int64 = int64(point2.X) - int64(point1.X)

            member this.C: int64 = -((int64(point1.X) * int64(point2.Y)) - (int64(point2.X) * int64(point1.Y)))
            member this.GetSteps = Math.Abs(point1.X - point2.X) + Math.Abs(point1.Y - point2.Y)

            override this.ToString() =
                "<" + string(this.Point1) + ", " + string(this.Point2) + ">"

        type Intersect(line1: Line, line2: Line) =
            member this.Line1: Line = line1
            member this.Line2: Line = line2

            member this.CalculateLineIntersection: Point =
                let det: int64 = (line1.A * line2.B) - (line1.B * line2.A)
                let detX: int64 = (line1.C * line2.B) - (line1.B * line2.C)
                let detY: int64 = (line1.A * line2.C) - (line1.C * line2.A)

                if int(det) <> 0 then
                    let x = int(detX / det)
                    let y = int(detY / det)

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
            result

        let getIntersections input1 input2 =
            let t1 = traverse input1
            let t2 = traverse input2
            seq {for i in 0 .. (t1.Length - 2) do
                    for j in 0 .. (t2.Length - 2) do
                        let line1 = Line(t1.[i], t1.[i + 1]);
                        let line2 = Line(t2.[j], t2.[j + 1]);
                        let intersect = Intersect(line1, line2)
                        let intersection = intersect.CalculateSegmentIntersection;
                        yield intersection }

        let minCentralPortDistance input1 input2 =
              let intersections = getIntersections input1 input2
              let t1 = traverse input1
              let t2 = traverse input2

              let intersections = seq {for i in 0 .. (t1.Length - 2) do
                                          for j in 0 .. (t2.Length - 2) do
                                              let line1 = Line(t1.[i], t1.[i + 1]);
                                              let line2 = Line(t2.[j], t2.[j + 1]);
                                              let intersect = Intersect(line1, line2)
                                              let intersection = intersect.CalculateSegmentIntersection;
                                              yield intersection }

              let minDistance = intersections
                                |> Seq.filter (isNull >> not)
                                |> Seq.filter (fun point -> point.X <> 0 && point.Y <> 0)
                                |> Seq.map (fun point -> Math.Abs(point.X) + Math.Abs(point.Y))
                                |> Seq.min
              minDistance

        let minTotalStepsForIntersections input1 input2 =
            let t1 = traverse input1
            let t2 = traverse input2

            let lines1 = seq { for i in 0 .. (t1.Length - 2) do
                                yield Line(t1.[i], t1.[i + 1]) } |> Seq.toList
            let lines2 = seq { for i in 0 .. (t2.Length - 2) do
                                yield Line(t2.[i], t2.[i + 1]) } |> Seq.toList

            let intersections = seq {for i in 0 .. (t1.Length - 2) do
                                        for j in 0 .. (t2.Length - 2) do
                                            let line1 = Line(t1.[i], t1.[i + 1]);
                                            let line2 = Line(t2.[j], t2.[j + 1]);
                                            let intersect = Intersect(line1, line2)
                                            let intersection = intersect.CalculateSegmentIntersection;
                                            yield intersection }

            let segmentIntersections = intersections
                                       |> Seq.filter (isNull >> not)
                                       |> Seq.filter (fun point -> point.X <> 0 && point.Y <> 0)
                                       |> Seq.toList


            let isPointInBetween (point1: Point) (point2: Point) (targetPoint: Point) =
                let distance (a: Point) (b: Point) =
                    Math.Sqrt(Math.Pow(float(a.X - b.X), 2.0) + Math.Pow(float(a.Y - b.Y), 2.0))

                (distance point1 targetPoint) + (distance targetPoint point2) = (distance point1 point2)

            let rec listPathToIntersections (lines: List<Line>) (intersections: List<Point>) (prev: List<Line>) (acc: List<List<Line>>) =
                match lines with
                | [] -> acc |> List.map (List.rev)
                | head::tail when let matched = intersections |> List.tryFind (fun intersection -> isPointInBetween head.Point1 head.Point2 intersection );
                                  match matched with
                                  | Some p -> true
                                  | None -> false
                    -> let intersectedLines = segmentIntersections
                                              |> List.filter (fun intersection -> isPointInBetween head.Point1 head.Point2 intersection )
                                              |> List.map (fun intersection -> Line(head.Point1, intersection))
                       let dd = intersectedLines |> List.map (fun x -> x::prev)
                       listPathToIntersections tail intersections (head::prev) (dd @ acc)
                | head::tail -> listPathToIntersections tail intersections (head::prev) acc

            let intersectPath1 = listPathToIntersections lines1 segmentIntersections [] []
            let intersectPath2 = listPathToIntersections lines2 segmentIntersections [] []

            let getTotalSteps (intersects: List<List<Line>>) = intersects |> List.map (fun o -> ((o |> List.last).Point2, o |> List.sumBy (fun i -> i.GetSteps)))

            let totals1 = getTotalSteps intersectPath1
            let totals2 = getTotalSteps intersectPath2

            let groupedTotals = (totals1 @ totals2) |> List.groupBy ( fun (k, v) -> k )

            let totalsByIntersectionPoints = groupedTotals |> List.map (fun (g, l) -> l |> List.sumBy (fun (k, v) -> v))
            List.min totalsByIntersectionPoints

        type Day3(filePath) =
            inherit AdventDayBase(filePath)

            interface IAdventDay with
                member this.PrintResults = let lines = this.ReadCommaSeparatedLines |> Seq.toList;
                                           printfn "Closest Central Port Distance: %d" (minCentralPortDistance (lines.[0]) (lines.[1]))
                                           printfn "Min Intersection Steps: %d" (minTotalStepsForIntersections (lines.[0]) (lines.[1]))