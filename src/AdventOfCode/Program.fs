namespace AdventOfCode

open System
open System.IO


module program =
    let result =
        File.ReadAllLines("inputs/day8.txt")
        |> SevenSegmentSearch.parse
        |> SevenSegmentSearch.getOutputValueForEntries

    printfn "%A" result