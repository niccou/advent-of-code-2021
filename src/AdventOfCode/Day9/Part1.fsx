let sample = [
    "2199943210"
    "3987894921"
    "9856789892"
    "8767896789"
    "9899965678"
]

let lines = System.IO.File.ReadAllLines($"{__SOURCE_DIRECTORY__}\Input.txt")

let input = lines

let parse (lines: string seq) =
    lines
    |> Seq.map (fun line ->
        line.ToCharArray()
        |> Seq.map (fun c -> [|c|])
        |> Seq.map (fun s -> System.String s)
        |> Seq.map int
        |> List.ofSeq
        )
    |> List.ofSeq

let map = input |> parse

let upperNeighbor (map: int list list) (row: int) (column: int) =
    if row = 0 then
        None
    else
        Some(map.[row-1].[column])

let bottomNeighbor (map: int list list) (row: int) (column: int) =
    if (row+1) = (map |> List.length)  then
        None
    else
        Some(map.[row+1].[column])

let leftNeighbor (map: int list list) (row: int) (column: int) =
    if column = 0 then
        None
    else
        Some(map.[row].[column-1])

let rightNeighbor (map: int list list) (row: int) (column: int) =
    if (column+1) = (map |> List.head |> List.length)  then
        None
    else
        Some(map.[row].[column+1])

let neighbors (map: int list list) (row: int) (column: int) =
    let haut = upperNeighbor map row column
    let bas = bottomNeighbor map row column
    let gauche = leftNeighbor map row column
    let droite = rightNeighbor map row column

    [haut; bas; gauche; droite] |> Seq.choose id

let findLowerPoints (map: int list list) =
    let rows = (map |> List.length) - 1
    let columns = (map |> List.head |> List.length) - 1

    seq {
        for row in 0..rows do
            for column in 0..columns do
                let current =  map.[row].[column]
                let lowestNeigbhor = (neighbors map row column) |> Seq.min
                if lowestNeigbhor <= current then
                    yield None
                else
                    yield Some(current)

    }
    |> Seq.choose id


input |> parse |> findLowerPoints |> Seq.map ((+)1) |> Seq.sum
