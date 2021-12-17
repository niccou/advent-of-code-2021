let input = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\\input.txt")

let sample =
    @"NNCB

    CH -> B
    HH -> N
    CB -> H
    NH -> C
    HB -> C
    HC -> B
    HN -> C
    NN -> C
    BH -> H
    NC -> B
    NB -> B
    BN -> B
    BB -> N
    BC -> B
    CC -> N
    CN -> C"
    |> (fun s -> s.Split("\n"))
    |> Array.map (fun s -> s.Trim())
    |> Seq.ofArray

let parse (lines: string seq) =
    let template = lines |> Seq.head
    let rules =
        lines
        |> List.ofSeq
        |> List.skip 2
        |> List.map (fun s -> s.Split(" -> ") |> fun xs -> (xs[0], xs[1]))
        |> Map.ofList

    template, rules

let (s0, rules) = sample |> parse

let step (str: string) =
    let strs = str.ToCharArray() |> List.ofSeq

    strs
    |> List.windowed 2
    |> List.map (fun chs -> chs |> Array.ofList |> System.String |> fun s -> match rules.TryFind s with | None -> [s[0]] | Some md -> [s[0];md[0]])
    |> List.concat
    |> fun prep -> List.append prep [(strs |> List.last)]
    |> Array.ofList
    |> System.String



let reslstr = [1..10] |> List.fold (fun acc _ -> step acc) s0
let reslcounts = reslstr.ToCharArray() |> List.ofArray |> List.countBy id

let mostCommon = reslcounts |> List.sortByDescending snd |> List.head
let lessCommon = reslcounts |> List.sortBy snd |> List.head

let res1 = (snd mostCommon) - (snd lessCommon)

// let s0smart = 