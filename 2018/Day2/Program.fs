open X0R.F.Files

let exactlyNLetter n str =
    str 
    |> Seq.countBy (fun x -> x)
    |> Seq.map (fun grp -> snd grp)
    |> Seq.filter (fun count -> count = n)
    |> Seq.length >= 1

let exactly2Letter str = exactlyNLetter 2 str
let exactly3Letter str = exactlyNLetter 3 str

let part1 filename = 
    let products = 
        filename
        |> Files.readLines
        |> Seq.map (fun line -> (exactly2Letter line, exactly3Letter line))
        |> Seq.fold (fun acc next -> match next with
                                     | (true, true) -> ((fst acc) + 1, (snd acc) + 1)
                                     | (true, false) -> ((fst acc) + 1, (snd acc))
                                     | (false, true) -> ((fst acc), (snd acc) + 1)
                                     | (false, false) -> acc) (0, 0)
    (fst products) * (snd products)

[<EntryPoint>]
let main argv =
    Seq.last argv
    |> part1
    |> printf "%i\r\n"

    0 // return an integer exit code
