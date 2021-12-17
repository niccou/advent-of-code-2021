let input = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\\input.txt")

let parse (input: string seq) =
    let col1 =
        input
        |> Seq.map (fun s -> s.Substring(0,5).Trim() |> int)
        |> Seq.chunkBySize 3
        |> Seq.map (fun s -> s |> Array.sort)
        |> List.ofSeq

    let col2 =
        input
        |> Seq.map (fun s -> s.Substring(5,5).Trim() |> int)
        |> Seq.chunkBySize 3
        |> Seq.map (fun s -> s |> Array.sort)
        |> List.ofSeq

    let col3 =
        input
        |> Seq.map (fun s -> s.Substring(10,5).Trim() |> int)
        |> Seq.chunkBySize 3
        |> Seq.map (fun s -> s |> Array.sort)
        |> List.ofSeq

    col1 @ col2 @ col3

let possibles (input: (int array) seq) =
    input |> Seq.filter (fun d -> d[0] + d[1] > d[2])

input |> parse |> possibles |> Seq.length