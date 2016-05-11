module LexicalDedupe // 316. Remove Duplicate Letters

let lexicalRemoveDedupe letters =
    let getCounts letters =
        let rec loop letters i mp = 
            match letters with
            | l :: r ->
                let value =
                    match Map.tryFind l mp with
                    | Some(v) -> i :: v
                    | None -> [i]

                loop r (i+1) (Map.add l value mp)
            | _ -> mp

        loop letters 0 Map.empty

    let counts = getCounts letters
    counts