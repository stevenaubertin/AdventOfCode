open X0R.F.Files

[<EntryPoint>]
let main argv =
    let rec computeFrequence startingValue values =
        match values with
        | [] -> startingValue
        | head::tail -> computeFrequence (startingValue + head) tail

    Seq.last argv
    |> Files.readLines 
    |> Seq.map int
    |> Seq.toList
    |> computeFrequence 0 
    |> printfn "%A"

    0 // return an integer exit code
