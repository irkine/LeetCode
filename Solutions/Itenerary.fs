module Itenerary

// 332. Reconstruct Itinerary
let reconstruct depart tickets = 
    let legs =
        tickets
        |> List.groupBy (fun (f,_) -> f)
        |> List.map (fun (f,t) -> (f, t |> List.map snd |> List.sort))
        |> Map.ofList

    let rec loop depart remainingLegs route =
        match remainingLegs |> Map.tryFind depart with
        | Some(arrive :: remaining) -> loop arrive (remainingLegs |> Map.add depart remaining) (depart :: route)
        | _ -> (depart :: route) |> List.rev

    loop depart legs []