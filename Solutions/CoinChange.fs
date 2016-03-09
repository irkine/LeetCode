module CoinChange // 322. Coin Change

let MakeChange coins amount =
    let rec loop coins remainingAmount change =
        let partial =
            match coins with
            | c :: r when c <= remainingAmount -> loop coins (remainingAmount - c) (c :: change)
            | c :: r -> loop r remainingAmount change
            | _ -> (remainingAmount, change)

        match coins with
        | c :: r when fst partial > 0 -> loop r remainingAmount change
        | _ -> partial

    let (ra, ch) = loop (List.sortBy (~-) coins) amount []
    if ra = 0 then ch
    else []
