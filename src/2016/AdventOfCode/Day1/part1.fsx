let input = System.IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}\\input.txt")

let sample = @"R5, L5, R5, R3, R18, R1, R1, R1, L17"

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

let solve input =
    let startDirection = N
    let startPosition = (0, 0)
    let plan = parse input

    let rec nextPosition (x, y) direction turns =
        if turns |> Seq.isEmpty then
            (x, y)
        else
            let turn = turns |> Seq.head

            let (newPosition, newDirection) = getNewPosition (x,y) direction turn

            nextPosition newPosition newDirection (turns |> Seq.tail)

    let (x, y) = nextPosition startPosition startDirection (plan |> List.ofSeq)

    System.Math.Abs(x) + System.Math.Abs(y) 

solve input