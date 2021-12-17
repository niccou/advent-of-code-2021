let sample =
    @"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"
    |> (fun s -> s.Split("\n"))
    |> Seq.ofArray

let input = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\input.txt")

type Stack = StackContents of char list

let push x aStack =   
    let (StackContents contents) = aStack
    let newContents = x::contents
    StackContents newContents

let pop (StackContents contents) = 
    match contents with 
    | top::rest -> 
        let newStack = StackContents rest
        (top,newStack)
    | [] -> failwith "Stack underflow"

let closingTags = [ ')'; ']'; '}'; '>' ]

let openingTag closingTag = 
    match closingTag with
    |')'-> '('
    |']'-> '['
    |'}'-> '{'
    |'>'-> '<'
    | _ -> ' '

let closingTag openingTag = 
    match openingTag with
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    |  _  -> ' '

let parse inputs =
    inputs
    |> Seq.map (fun (s: string) -> s.ToCharArray())

let firstIllegal (line: char seq) =
    let startingStack = StackContents []

    let rec Check (stack:Stack) inputs =
        match inputs with
        | [] -> None
        | head::tail ->
            if closingTags |> Seq.contains head then
                let lastOpenTag, poppedStack = pop stack 
                if (openingTag head) = lastOpenTag then
                    Check poppedStack tail
                else
                    Some(head)
            else
                Check (push head stack) tail


    Check startingStack (line|>List.ofSeq)

let scoreIllegal character =
    match character with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    |  _  -> 0

let incompleteLine (line: char seq) = Option.isNone (firstIllegal line)

let scoreLegal character =
    match character with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    |  _  -> 0L

let scoreCloseTags (StackContents stack) =
    stack |> List.map (closingTag>>scoreLegal) |> List.fold (fun acc score -> (acc * 5L) + score) 0L


let removeValidChunk (line: char seq) =
    let startingStack = StackContents []

    let rec Check (stack:Stack) inputs =
        match inputs with
        | [] -> stack
        | head::tail ->
            if closingTags |> Seq.contains head then
                let _, poppedStack = pop stack 
                Check poppedStack tail
            else
                Check (push head stack) tail

    Check startingStack (line|>List.ofSeq)

let findMiddleScore (scores: int64 seq) =
    let indexedScores = scores |> Seq.toList |> List.sort
    let middle = (((indexedScores |> List.length) - 1) / 2)
    indexedScores.[middle]

input
|> parse
|> Seq.filter incompleteLine
|> Seq.map removeValidChunk
|> Seq.map scoreCloseTags
|> findMiddleScore




