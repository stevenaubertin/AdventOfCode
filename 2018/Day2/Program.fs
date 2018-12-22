open X0R.F.Files

let exactlyNLetter n str =
    str 
    |> Seq.countBy (fun x -> x)
    |> Seq.map (fun grp -> snd grp)
    |> Seq.filter (fun count -> count = n)
    |> Seq.length
    |> Seq.a

let exactly2Letter str = exactlyNLetter 2 str
let exactly3Letter str = exactlyNLetter 3 str

[<EntryPoint>]
let main argv =
    //Seq.last argv
    @"C:\Users\saubertin\repo\adventofcode\2018\Day2\Test.txt"
    |> Files.readLines
    |> Seq.map (fun line -> (exactly2Letter line, exactly3Letter line))
    |> Seq.iter (fun x -> printfn "%A" x)
    //|> Seq.reduce (fun acc next -> ((fst acc) + (fst next), (snd acc) + (snd acc)))
    //|> printfn "%A"

    0 // return an integer exit code
