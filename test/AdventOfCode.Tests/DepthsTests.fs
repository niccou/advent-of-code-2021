namespace AdventOfCode.Tests

open Xunit
open AdventOfCode

module DepthsTests =
    [<Fact>]
    let ``The number of times a depth measurement increases is 7`` () =
        let depths = [| "199"; "200"; "208"; "210"; "200"; "207"; "240"; "269"; "260"; "263" |]

        let numberOfTimes = depths |> Depths.numberOfTimesADepthMeasurementIncreases
        
        Assert.Equal(7, numberOfTimes)

    [<Fact>]
    let ``The number of times the sum of measurements in this sliding window increases is 5`` () =
        let depths = [| "199"; "200"; "208"; "210"; "200"; "207"; "240"; "269"; "260"; "263" |]

        let numberOfTimes = depths |> Depths.numberOfTimesTheSumOfMeasurementsInSlidingWindow 3

        Assert.Equal(5, numberOfTimes)
