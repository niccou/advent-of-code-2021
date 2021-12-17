let input = System.IO.File.ReadAllText($"{__SOURCE_DIRECTORY__}\\input.txt")

let sample = "38006F45291200"

let decoder = 
    [
        ("0", "0000")
        ("1", "0001")
        ("2", "0010")
        ("3", "0011")
        ("4", "0100")
        ("5", "0101")
        ("6", "0110")
        ("7", "0111")
        ("8", "1000")
        ("9", "1001")
        ("A", "1010")
        ("B", "1011")
        ("C", "1100")
        ("D", "1101")
        ("E", "1110")
        ("F", "1111")
    ] |> Map.ofSeq

let decode decoder (str: string) =
    str.ToCharArray()
    |> Seq.map (fun c -> c.ToString())
    |> Seq.map (fun s -> decoder |> Map.find s)

decode decoder sample