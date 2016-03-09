module TwoNonDuplicated // ###. ???

let nonDupeNaive numbers =
    numbers
    |> Seq.countBy (fun i -> i)
    |> Seq.choose(fun (i, c) -> if c = 1 then Some(i) else None)

let nonDupeFast set = 
    let fullxor = set |> Seq.reduce (fun a i -> a ^^^ i)
    
    let slot =
        Seq.initInfinite (fun i -> 1 <<< i)
        |> Seq.find (fun i -> i &&& fullxor = i)

    let n1 =
        set
        |> Seq.fold(fun a i -> if i &&& slot = slot then a ^^^ i else a) 0

    let n2 = n1 ^^^ fullxor

    (n1, n2)

// SAMPLE INPUT [| 1; 2; 1; 5; 4; 4; 3; 5 |] -> 2,3