namespace X0R.F.Seq

module Seq =
    // This cycle function was taken from : 
    // http://www.fssnip.net/a5/title/Generate-a-repeating-infinite-sequence-
    let rec cycle xs =
        seq { yield! xs; yield! cycle xs }

    let first xs =
        Seq.take 1 xs