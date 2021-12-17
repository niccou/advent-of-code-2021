let input = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\\input.txt")

let sample =
    @"aaaaa-bbb-z-y-x-123[abxyz]
      a-b-c-d-e-f-g-h-987[abcde]
      not-a-real-room-404[oarel]
      totally-real-room-200[decoy]".Split("\n", System.StringSplitOptions.TrimEntries)
    |> Seq.ofArray

let parse (line: string) =
    let datas = line.Split([|'-';'[';']'|],System.StringSplitOptions.RemoveEmptyEntries)
    let name =
        datas
        |> Array.take ((datas |> Array.length)-2)
        |> Array.fold (fun acc s -> acc + s) ""
        |> fun s -> s.ToCharArray()
        |> Seq.groupBy id
        |> Seq.map (fun (k,v) -> (v |> Seq.length, k))

    let zone = datas |> Array.skip ((datas |> Array.length)-2) |> Array.head
    let checkSum = datas |> Array.last
    (name, zone, checkSum)

let calculatCheckSumForName (name: seq<int*char>) =
    name
    |> Seq.groupBy fst
    |> Seq.map (fun (k,v) -> (k, v|> Seq.map snd |> Seq.sort))
    |> Seq.sortByDescending fst
    |> Seq.map (fun (_, v) -> v)
    |> Seq.fold (fun acc c -> c |> Seq.append acc ) [||]
    |> Seq.take 5
    |> Array.ofSeq
    |> (fun c -> System.String(c))

let isValidCheckSum name checkSum =
    let calculated = calculatCheckSumForName name
    calculated = checkSum

let solve input =
    input
    |> Seq.map parse
    |> Seq.filter (fun (name, _, checkSum) -> isValidCheckSum name checkSum)
    |> Seq.sumBy (fun (_, zone, _) -> zone |> int)

solve input