open X0R.F.Files

let part1 filepath =
    filepath
    |> Files.readLines 
    |> Seq.map int
    |> Seq.sum
    |> printfn "%A"

let part2 filepath =
    // This cycle function was taken from : http://www.fssnip.net/a5/title/Generate-a-repeating-infinite-sequence-
    let rec cycle xs =
        seq { yield! xs; yield! cycle xs }
    
    let mutable seen = [0]

    filepath
    |> Files.readLines 
    |> Seq.map int
    |> Seq.cache
    |> cycle
    |> Seq.takeWhile (fun x -> Seq.fin)
    |> printfn "%A"

let tmp argv = 

    let computeFrequence values =
        let rec computeImplementation current mem rest =
            let newmem = Seq.append mem [current] |> Seq.toList
            let count = newmem
                        |> Seq.filter (fun x -> current = x)
                        |> Seq.length
            match count with
            | 2 -> current
            | _ -> match rest with
                   | [] -> current
                   | head::tail -> computeImplementation (current + head) newmem tail
        computeImplementation 0 [] values
    
    Seq.last argv
    |> Files.readLines 
    |> Seq.map int
    |> Seq.toList
    |> computeFrequence
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    Seq.last argv 
    |> part2
    0 // return an integer exit code
