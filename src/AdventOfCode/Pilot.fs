namespace AdventOfCode

type Direction =
    | Forward
    | Up
    | Down

module Pilot =
    let sumSteps = 
        Array.map (fun (s: string) -> s.Split(' ').[1])
        >> Array.map int
        >> Array.sum

    let isForward (step: string) = step.StartsWith("forward")
    let isUp (step: string) = step.StartsWith("up")

    let directionFromString (direction: string) =
        match direction with
        | "forward" -> Some(Forward)
        | "up" -> Some(Up)
        | "down" -> Some(Down)
        | _ -> None


    let splitSteps steps =
        let (forwardSteps, depthSteps) =
             steps |> Array.partition isForward

        let (upSteps, downSteps) = depthSteps |> Array.partition isUp
        (forwardSteps, upSteps, downSteps)

    let getFinalPosition (steps: string array) = 
        let (forwardSteps, upSteps, downSteps) = steps |> splitSteps

        let position = forwardSteps |> sumSteps

        let up = upSteps |> sumSteps
        let down = downSteps |> sumSteps
        (position, (down - up))

    let getFinalPositionWithAim (steps: string array) = 
        let splitStep (step: string) =
            let split = step.Split(' ')
            ((split.[0] |> directionFromString), (split.[1] |> int))

        let rec followSteps (position: int) (depth: int) (aim: int) remainSteps =
            match remainSteps with
            | [] -> (position, depth)
            | (head::tail) ->
                let (direction, value) = head |> splitStep
                match direction with
                | Some(Up) -> 
                    let newAim = aim - value
                    followSteps position depth newAim tail
                | Some(Down) -> 
                    let newAim = aim + value
                    followSteps position depth newAim tail
                | Some(Forward) -> 
                    let newPosition = position + value
                    let newDepth = depth + aim * value
                    followSteps newPosition newDepth aim tail
                | None -> followSteps position depth aim tail

        followSteps 0 0 0 (steps |> List.ofArray)

    let getPositionalValue steps =
        let (position, depth) = steps |> getFinalPosition
        position * depth

    let getAimValue steps =
        let (position, depth) = steps |> getFinalPositionWithAim
        position * depth