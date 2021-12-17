let input = System.IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}\\input.txt")

let sample = @"R8, R4, R4, R8"

type Direction = N | S | W | E

type Turn = R of int | L of int

let parseTurn (s: string) =
    let (c, distance) = (s.Substring(0, 1), s.Substring(1) |> int)
    if c = "R" then
        R distance
    else
        L distance

let parse (input: string) = 
    input.Split(",", System.StringSplitOptions.TrimEntries)
    |> Seq.map parseTurn

let turnTo direction turn =
    match (direction, turn) with
    | (N, L _) -> W
    | (W, L _) -> S
    | (S, L _) -> E
    | (E, L _) -> N
    | (N, R _) -> E
    | (E, R _) -> S
    | (S, R _) -> W
    | (W, R _) -> N

let getNewPosition (x,y) direction turn = 
    let newDirection = turnTo direction turn
    let distance = 
        match turn with
        | R d -> d
        | L d -> d

    match newDirection with
    | N -> (x - distance, y), N
    | S -> (x + distance, y), S
    | E -> (x, y + distance), E
    | W -> (x, y- distance), W

let getSegment (x, y) (nextX, nextY) =
    [
        if x = nextX then
            if y < nextY then
                for a in (y+1)..nextY -> (x, a)
            else
                for a in (y-1).. -1 ..nextY -> (x, a)
        else
            if x < nextX then
                for a in (x+1)..nextX -> (a, y)
            else
                for a in (x-1).. -1 ..nextX -> (a, y)
    ]

let tryFindFirstIntersect visited location nextLocation =
    let segment = getSegment location nextLocation

    let intersect = segment |> List.tryFind (fun loc -> visited |> Set.contains loc)

    let addedVisited =
        (visited |> Set.toList) @ segment
        |> Set.ofList

    match intersect with 
    | Some(location) -> Some(location), addedVisited
    | None -> None, addedVisited

let solve input =
    let startPosition = (0, 0)
    let startVisited = [startPosition] |> Set.ofList
    let startDirection = N
    let plan = parse input

    let rec nextPosition (x, y) direction turns visited =
        printfn "Location : %A" (x, y)
        printfn "Direction : %A" direction
        printfn "Turns : %A" turns
        printfn "Visited : %A" visited

        if turns |> Seq.isEmpty then
            (x, y)
        else
            let turn = turns |> Seq.head

            let (newPosition, newDirection) = getNewPosition (x,y) direction turn

            let (intesect, newVisited) = tryFindFirstIntersect visited (x,y) newPosition

            match intesect with
            | Some(location) -> location
            | None -> nextPosition newPosition newDirection (turns |> Seq.tail) newVisited

    let (x, y) = nextPosition startPosition startDirection (plan |> List.ofSeq) startVisited

    System.Math.Abs(x) + System.Math.Abs(y) 

solve input