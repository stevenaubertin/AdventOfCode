open X0R.F.Files
open X0R.F.Seq

let part1 filepath =
    Files.readLines filepath
    |> Seq.map int
    |> Seq.sum
    |> printfn "%A"

let part2 filepath =
    let computeRecurringFrequency xs =
        let rec implementation acc rest mem =
            let freq = acc + (List.last mem)
            match List.tryFind (fun x -> x = freq) mem with
            | Some x -> x
            | None -> match rest with
                      | [] -> implementation (List.head xs) (List.tail xs) (List.append mem [freq])
                      | _ -> implementation (List.head rest) (List.tail rest) (List.append mem [freq])
        match xs with
        | [] -> None
        | _ -> Some (implementation (List.head xs) (List.tail xs) [0])

    Files.readLines filepath 
    |> Seq.toList
    |> List.map int
    |> computeRecurringFrequency
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    Seq.last argv 
    |> part2
    0 // return an integer exit code
