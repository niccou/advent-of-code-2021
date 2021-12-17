namespace AdventOfCode.Tests

open Xunit
open AdventOfCode

module BinaryDiagnosticsTests =
    let diagnostic =
        [|
            "00100";
            "11110";
            "10110";
            "10111";
            "10101";
            "01111";
            "00111";
            "11100";
            "10000";
            "11001";
            "00010";
            "01010"
        |]

    [<Fact>]
    let ``Gamma rate should be 22`` () =
        let (gammaRate, _) = BinaryDiagnostics.getRates diagnostic

        Assert.Equal(22, gammaRate)

    [<Fact>]
    let ``Epsilon rate should be 9`` () =
        let (_, espilonRate) = BinaryDiagnostics.getRates diagnostic

        Assert.Equal(9, espilonRate)

    [<Fact>]
    let ``Power consumption should be 198`` () =
        let powerConsumption = BinaryDiagnostics.getPowerConsumption diagnostic

        Assert.Equal(198, powerConsumption)
