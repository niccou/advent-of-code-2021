namespace AdventOfCode.Tests

open Xunit
open AdventOfCode

module PilotTests =
    let steps = [| "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" |]
    [<Fact>]
    let ``You would have a horizontal position of 15`` () =
        let (position, _) = Pilot.getFinalPosition steps

        Assert.Equal(15, position)

    [<Fact>]
    let ``You would have a depth of 10`` () =
        let (_, depth) = Pilot.getFinalPosition steps

        Assert.Equal(10, depth)

    [<Fact>]
    let ``You would have a result of 150`` () =
        let result = Pilot.getPositionalValue steps

        Assert.Equal(150, result)

    [<Fact>]
    let ``You would have a horizontal position with aim of 15`` () =
        let (position, _) = Pilot.getFinalPositionWithAim steps

        Assert.Equal(15, position)

    [<Fact>]
    let ``You would have a depth with aim of 60`` () =
        let (_, depth) = Pilot.getFinalPositionWithAim steps

        Assert.Equal(60, depth)

    [<Fact>]
    let ``You would have a result of 900`` () =
        let result = Pilot.getAimValue steps

        Assert.Equal(900, result)
