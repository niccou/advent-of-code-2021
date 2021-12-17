let input = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\\input.txt")

let sample =
    @"6,10
        0,14
        9,10
        0,3
        10,4
        4,11
        6,0
        6,12
        4,1
        0,13
        10,12
        3,4
        3,0
        8,4
        1,10
        2,14
        8,10
        9,0

        fold along y=7
        fold along x=5"
    |> (fun s -> s.Split("\n"))
    |> Array.map (fun s -> s.Trim())
    |> Seq.ofArray

type Direction = Up | Left

let parseDots (lines: string seq) = 
    lines
    |> Seq.takeWhile (fun s -> s <> "")
    |> Seq.map (fun s ->
        let [|x;y|] = s.Split(',')
        (x |> int, y |> int)
        )
    |> Set.ofSeq

let toDirection (s:string): Direction =
    if (s = "x") then Left
    else Up

let parseInstructions (lines: string seq) = 
    let startString = "fold along"
    lines
    |> Seq.filter (fun s -> s.StartsWith(startString))
    |> Seq.map (fun (s: string) -> s.Substring(startString |> String.length).Trim())
    |> Seq.map (fun (s: string) ->
        let [|direction; value|] = s.Split("=")
        (direction |> toDirection, value |> int)
        )

let nextPosition (direction, foldIndex) (x, y) =
    match direction with
    | Left -> 
        (foldIndex - (x - foldIndex), y)
    | Up -> 
        (x, foldIndex - (y - foldIndex))

let dotsThatDontMove (direction, foldIndex) (dots: Set<int*int>)= 
    dots
    |> Set.filter (fun (x, y) -> 
        match direction with
        | Left -> 
            x < foldIndex
        | Up -> 
            y < foldIndex
        )

let dotsThatWillMove (direction, foldIndex) (dots: Set<int*int>)= 
    dots
    |> Set.filter (fun (x, y) -> 
        match direction with
        | Left -> 
            x > foldIndex
        | Up -> 
            y > foldIndex
        )

let rec foldDots instructions (dots: Set<int*int>)=
    match instructions |> Seq.toList with
    | [] -> dots
    | _ ->
        let (direction, foldIndex) = instructions |> Seq.head
        let standByDots = dots |> dotsThatDontMove (direction, foldIndex)

        let movingDots = dots |> dotsThatWillMove (direction, foldIndex) |> Set.map (nextPosition (direction, foldIndex))
        
        let newDots = ((movingDots |> Set.toList) @ (standByDots |> Set.toList)) |> Set.ofList

        foldDots (instructions |> Seq.tail) newDots

let dots = input |> parseDots
let instructions = input |> parseInstructions

let display (dots: Set<int*int>) =
    let horizontalDimension = dots |> Set.map (fun (x,_) -> x) |> Set.maxElement
    let verticalDimension = dots |> Set.map (fun (_,y) -> y) |> Set.maxElement
    for y in 0..verticalDimension do
        printfn ""
        for x in 0..horizontalDimension do
            if dots |> Set.contains (x, y) then
                printf "#"
            else
                printf "."

foldDots instructions dots |> display

