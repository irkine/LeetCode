module HouseRobber // 198. House Robber

let rob houses =
    let rec rob robThis dontRobThis houses =
        match houses with
        | this::rest ->
            let bestOption = max dontRobThis (robThis + this)
            rob dontRobThis bestOption rest

        | [] -> max robThis dontRobThis

    rob 0 0 houses