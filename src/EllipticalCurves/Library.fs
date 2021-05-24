namespace Library

module EllipticalCurves =
    open System.Collections.Generic
    open System.Text
    open System.Linq

    type Curve(a : int, b : int, p : int) =
        member this.A = a
        member this.B = b
        member this.P = p
        
        override this.GetHashCode() =
            hash (this.A, this.B, this.P)
            
        override this.Equals(b) =
            match b with
            | :? Curve as c -> (this.A, this.B, this.P) = (c.A, c.B, c.P)
            | _ -> false

    type Point(x : int, y : int, curve : Curve) =
        override this.ToString() =
            if this.IsZeroPoint
            then
                "   O  "
            else
                $"({this.X}, {this.Y})"
        
        override this.GetHashCode() =
            hash (this.X, this.Y)
            
        override this.Equals(b) =
            match b with
            | :? Point as p -> (this.X, this.Y) = (p.X, p.Y)
            | _ -> false
        member this.X = (x % curve.P + curve.P) % curve.P
        member this.Y = (y % curve.P + curve.P) % curve.P
        member this.Curve = curve
        member this.IsZeroPoint = this.X = 0 && this.Y = 0
        
        static member (+) (point1 : Point, point2 : Point) =
            if point1.Curve <> point2.Curve
            then
                failwith "Different curves"
                
            let curve = point1.Curve
            let p = point1.Curve.P
            let a = point1.Curve.A
            let b = point1.Curve.B
            
            let reverse x =
                if x = 0
                then
                    0
                else
                    let x = (x % p + p) % p
                    let mutable i = 1 
                    while (x * i) % p <> 1 do
                        i <- i + 1
                    i
                    
            let negative (point : Point) =
                Point(point.X, -point.Y, point.Curve)
                
            let lambda x1 x2 y1 y2 =
                if x1 = x2 
                then
                    ((3 * x1 * x1 + a) % p * reverse (2 * y1)) % p
                else
                    ((y2 - y1) % p * reverse (x2 - x1)) % p
                    
            let mu x1 x2 y1 y2 =
                if x1 = x2
                then
                    ((-x1 * x1 * x1 + a * x1 + 2 * b) % p * reverse (2 * y1)) % p
                else
                    ((y1 * x2 - y2 * x1) % p * reverse (x2 - x1)) % p
                    
            let x1 = point1.X
            let x2 = point2.X
            let y1 = point1.Y
            let y2 = point2.Y
                
            if point1.IsZeroPoint
            then
                point2
            elif point2.IsZeroPoint
            then
                point1
            elif point1 = negative point2
            then
                Point(0, 0, curve)
            else
                let x3 = ((lambda x1 x2 y1 y2) * (lambda x1 x2 y1 y2) - x2 - x1) % p
                let y3 = (-(lambda x1 x2 y1 y2) * x3 - (mu x1 x2 y1 y2)) % p
                Point(x3, y3, curve)
        
    let getSpace count =
        let mutable space = ""
        for i in [0 .. count] do
            space <- space + " "
        space
            
    let showList (collection : List<_>) =
        let stringCollection = collection.Select(fun el -> el.ToString()).ToList()
        let maxLength = stringCollection.Select(fun el -> el.Length).Max() 
        let sb = StringBuilder()
        stringCollection.ForEach(fun el -> sb.Append(el + (getSpace (maxLength - el.Length))) |> ignore)
        sb.ToString()
        
    let showTable (table : List<List<Point>>) =
        let sb = StringBuilder()
        for line in table do
            sb.Append((showList line) + "\n") |> ignore
        sb.ToString()
        
    let getIntersection (collection1 : IEnumerable<int>) (collection2 : IEnumerable<int>) =
        collection1.Intersect collection2 |> List<int>
            
    let getPairsForOne (x : int) (xPolynomial : int) (yPairs : IEnumerable<int * int>)=
        yPairs
            .Where(fun (_, ySquared) -> ySquared = xPolynomial)
            .Select(fun (y, _) -> (x, y))
            .ToList()
            
    let getPairs (xPairs : IEnumerable<int * int>) (yPairs : IEnumerable<int * int>) =
        let intersection = getIntersection (yPairs.Select snd) (xPairs.Select snd)
        xPairs
            .Where(fun (x, xPolynomial) -> intersection.Contains xPolynomial)
            .Select(fun (x, xPolynomial) -> (getPairsForOne x xPolynomial yPairs))
            .SelectMany(fun e -> e.AsEnumerable())
            .ToList()
            
    let getPoints (pairs : IEnumerable<int * int>) curve =
        let zeroPoint = Point(0, 0, curve)
        let points = pairs.Select(fun (x, y) -> Point(x, y, curve))
        [zeroPoint].Concat(points).ToList()
            
    let getCreativeElement (points : List<Point>) =
        let alreadyReceived = List<Point>()
        let mutable i = 1
        while alreadyReceived.Count <> points.Count && i < points.Count do
            alreadyReceived.Clear()
            let mutable nextPoint = points.[i]
            while not (alreadyReceived.Contains(nextPoint)) do
                alreadyReceived.Add(nextPoint)
                nextPoint <- points.[i] + nextPoint
            i <- i + 1
        if alreadyReceived.Count = points.Count
        then
            Some(alreadyReceived.[0])
        else
            None
            
    let getCyclicProof (creativeElement : Point) (points : List<Point>) =
        let sb = StringBuilder()
        sb.Append($"Group is cyclic. Creative element: {creativeElement}\n") |> ignore
        sb.Append("Proof of cyclicity:\n") |> ignore
        let zeroPoint = points.[0]
        let mutable previousSum = creativeElement
        for i in [0 .. points.Count - 1] do
            sb.Append($"{creativeElement} + {previousSum} = {creativeElement + previousSum}\n") |> ignore
            previousSum <- creativeElement + previousSum
        sb.ToString()
            
    let getTable (points : List<Point>) =
        let sb = StringBuilder()
        let count = points.Count
        let table = points.Select(fun point -> List<Point>(points)).ToList()
        for i in [0 .. count - 1] do
            table.[i].[0] <- points.[i]
        sb.Append(showList table.[0] + "\n") |> ignore
        for i in [1 .. count - 1] do
            for j in [1 .. count - 1] do
                table.[i].[j] <- table.[0].[j] + table.[i].[0]
            sb.Append(showList table.[i] + "\n") |> ignore
        sb.ToString()

    let leftPart y p = (y * y) % p
    let rightPart x a b p = (x * x * x + a * x + b) % p
    let discriminant a b p = (27 * b * b + 4 * a * a * a) % p
    
    let getResult p a b =
        let sb = StringBuilder()
        let numbers = [0 .. p - 1]
        
        sb.AppendLine($"Discriminant: {discriminant a b p}") |> ignore
        sb.AppendLine() |> ignore
        
        let yPairs = List.map (fun y -> (y, leftPart y p)) numbers
        sb.AppendLine($"(y, y^2): {showList (yPairs.ToList())}") |> ignore
        sb.AppendLine() |> ignore
        
        let xPairs = List.map (fun x -> (x, rightPart x a b p)) numbers
        sb.AppendLine($"(x, x^3 + {a}x + {b}): {showList (xPairs.ToList())}") |> ignore
        sb.AppendLine() |> ignore
        
        let pairs = getPairs xPairs yPairs
        let curve = Curve(a, b, p)
        let points = getPoints pairs curve
        sb.AppendLine($"Points: {showList points}") |> ignore
        sb.AppendLine() |> ignore
        
        sb.AppendLine("Table of sums:") |> ignore
        sb.AppendLine(getTable points) |> ignore
        sb.AppendLine() |> ignore
        
        match getCreativeElement points with
        | Some(creativeElement) -> sb.AppendLine($"{getCyclicProof creativeElement points}") |> ignore
        | None -> sb.AppendLine("Group is not cyclic") |> ignore
        
        sb.ToString()