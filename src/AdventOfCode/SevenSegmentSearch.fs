module SevenSegmentSearch

open System

let parse (lines: string seq) =
    lines |> Seq.map (fun text -> text.Split("|", System.StringSplitOptions.RemoveEmptyEntries).[1].Trim())

let easyNumber (text: string) =
    match text.Length with
    | 2 -> Some("1")
    | 4 -> Some("4")
    | 3 -> Some("7")
    | 7 -> Some("8")
    | _ -> None

let countEasyNumbersForText (text: string) =
    text.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.choose easyNumber
    |> Seq.length

let folder (acc: int list) (lengths: int list) =
    acc @ lengths

let countEasyNumbers (lines: string seq) =
    lines
    |> Seq.map countEasyNumbersForText
    |> Seq.sum

let getOutputValue (text: string) =
    match text with
    | "cagedb" -> Some("0")
    | "ab" -> Some("1")
    | "gcdfa" -> Some("2")
    | "fbcad" -> Some("3")
    | "eafb" -> Some("4")
    | "cdfbe" -> Some("5")
    | "cdfgeb" -> Some("6")
    | "dab" -> Some("7")
    | "acedgfb" -> Some("8")
    | "cefabd" -> Some("9")
    | _ -> None

let getOutput (text: string) =
    text.ToCharArray()
    |> Array.sort
    |> (fun s -> new String(s))
    |> getOutputValue
    
let getOutputs (text: string) =
    text.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.choose getOutput
    |> Seq.fold (fun acc s -> sprintf "%s%s" acc s) ""
    |> Seq.head
    |> int

let getOutputValueForEntries (lines: string seq) =
    lines
    |> Seq.map getOutputs
    |> Seq.map (fun c -> 
        printfn "%A" c
        c)
    |> Seq.toArray
