let input = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\\input.txt")

let sample = 
    @"ULL
      RRDDD
      LURDL
      UUUUD".Split('\n', System.StringSplitOptions.TrimEntries)
    |> Seq.ofArray

let keypad =
    [
        ((0,2),"1")
        ((1,1),"2")
        ((1,2),"3")
        ((1,4),"4")
        ((2,0),"5")
        ((2,1),"6")
        ((2,2),"7")
        ((2,3),"8")
        ((2,4),"9")
        ((3,1),"A")
        ((3,2),"B")
        ((3,3),"C")
        ((4,2),"D")
    ]
    |> Map.ofList

let startLocation = (2,0)

let nextLocation (x, y) instruction =
    match instruction with
    | 'U' -> (x-1, y)
    | 'D' -> (x+1, y)
    | 'L' -> (x, y-1)
    | 'R' -> (x, y+1)
    | _ -> (x, y)

let nextButton keypad location instruction =
    let next = nextLocation location instruction
    match keypad |> Map.tryFind next with
    | Some(_) -> next
    | None -> location

let followInstruction keypad startLocation (instructions: string): (int*int)*string =
    let moves = instructions.ToCharArray() |> Seq.ofArray

    let rec follow location moves =
        if moves |> Seq.isEmpty then
            location
        else
            follow (nextButton keypad location (moves |> Seq.head)) (moves |> Seq.tail)

    let lastLocation = follow startLocation moves

    (lastLocation, keypad |> Map.find (lastLocation))

let rec followInstructions keys keypad startLocation (instructions: string seq) =
    if instructions |> Seq.isEmpty then
        keys |> Seq.rev |> Seq.map snd
    else
        let (location, key) = followInstruction keypad startLocation (instructions |> Seq.head)
        followInstructions (keys |> Seq.append [(location, key)]) keypad (location) (instructions |> Seq.tail)

followInstructions [] keypad startLocation input |> Seq.toList