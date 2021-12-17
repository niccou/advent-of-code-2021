let input = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\\input.txt")

let parse (input: string seq) =
    input
    |> Seq.map (fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Array.sort)

let possibles (input: (int array) seq) =
    input |> Seq.filter (fun d -> d[0] + d[1] > d[2])

input |> (parse>>possibles) |> Seq.length