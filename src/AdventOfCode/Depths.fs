namespace AdventOfCode

module Depths =
    let toIntList = Array.map int >> Array.toList
    let numberOfTimesADepthMeasurementIncreases (depths: string array) =
        let rec countIncrease count previousDepth remain =
            match remain with
            | [] -> count
            | head::tail when head > previousDepth
                -> countIncrease (count+1) head tail
            | head::tail -> countIncrease count head tail

        let depthsList = depths |> toIntList
        countIncrease 0 (depthsList |> List.head) (depthsList |> List.tail)

    let numberOfTimesTheSumOfMeasurementsInSlidingWindow (windowSize: int) (depths: string array) =
        depths
            |> Array.map int
            |> Array.windowed windowSize
            |> Array.map (fun m -> Array.sum m)
            |> Array.map string
            |> numberOfTimesADepthMeasurementIncreases
