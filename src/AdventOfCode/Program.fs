namespace AdventOfCode

open System.IO


module program =
    let depths = File.ReadAllLines("inputs/day1.txt")
    printfn "Day 1: the number of times a depth measurement increases is %i" (depths |> Depths.numberOfTimesADepthMeasurementIncreases)
    printfn "Day 1: the number of times the sum of measurements in this sliding window increases %i" (depths |> Depths.numberOfTimesTheSumOfMeasurementsInSlidingWindow 3)

    let steps = File.ReadAllLines("inputs/day2.txt")
    printfn "Day 2: Positional value %i" (steps |> Pilot.getPositionalValue)
    printfn "Day 2: Positional value with aim %i" (steps |> Pilot.getAimValue)
