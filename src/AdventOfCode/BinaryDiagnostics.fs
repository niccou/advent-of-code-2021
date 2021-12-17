namespace AdventOfCode

open System

type Counter = 
    {
        Zero: int
        One: int
    }

type Rates = 
    {
        Values1: Counter
        Values2: Counter
        Values3: Counter
        Values4: Counter
        Values5: Counter
    }

module BinaryDiagnostics =
    let byteArrayToInt (values: byte array) = BitConverter.ToInt32(values, 0)
    let updateCounter (counter: Counter) (c: char) =
        if c ='0' then
            {counter with Zero = counter.Zero + 1}
        else
            {counter with One = counter.One + 1}

    let defineUsedValue (values: Rates) (diagnostic: char array) =
        let values1 = updateCounter values.Values1 diagnostic.[0]
        let values2 = updateCounter values.Values2 diagnostic.[1]
        let values3 = updateCounter values.Values3 diagnostic.[2]
        let values4 = updateCounter values.Values4 diagnostic.[3]
        let values5 = updateCounter values.Values5 diagnostic.[4]

        {
            Values1 = values1
            Values2 = values2
            Values3 = values3
            Values4 = values4
            Values5 = values5
        }
            

    let defineMostUsedChar (counter: Counter) =
        if counter.Zero > counter.One then
            '0'
        else
            '1'
        
    let defineMostUsedChars (rates: Rates) =
        let char1 = rates.Values1 |> defineMostUsedChar
        let char2 = rates.Values2 |> defineMostUsedChar
        let char3 = rates.Values3 |> defineMostUsedChar
        let char4 = rates.Values4 |> defineMostUsedChar
        let char5 = rates.Values5 |> defineMostUsedChar
        [|char1; char2; char3; char4; char5|]
        

    let invert (c:char) =
        if c = '0' then
            '1'
        else
            '0'

    let getRates (diagnostic: string array) =
        let diag = diagnostic |> Array.map (fun d -> d.ToCharArray())

        let emptyRates =
            {
                Values1 = { Zero = 0; One = 0}
                Values2 = { Zero = 0; One = 0}
                Values3 = { Zero = 0; One = 0}
                Values4 = { Zero = 0; One = 0}
                Values5 = { Zero = 0; One = 0}
            }


        let valueToByte (c:char): byte=
            if c = '0' then
                byte 0
            else
                byte 1
        let convertToRate = Array.map valueToByte >> byteArrayToInt

        let used =
            diag
            |> Array.fold defineUsedValue emptyRates
            |> defineMostUsedChars

        let a = used |> Array.map valueToByte |> byteArrayToInt

        let notUsed = used |> Array.map invert

        let gammaRate = 
            used
            |> convertToRate
        let espilonRate =
            notUsed
            |> convertToRate


        (gammaRate, espilonRate)

    let getPowerConsumption (diagnostic: string array) =
        198

