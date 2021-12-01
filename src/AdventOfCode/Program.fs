namespace AdventOfCode

open System.IO


module program =
    let depths = File.ReadAllLines("inputs/day1.txt")
    printfn "%A" (depths |> Depths.numberOfTimesTheSumOfMeasurementsInSlidingWindow 3)
