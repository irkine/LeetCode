module Balloons2

type Balloon = {
    Value : int;
    mutable L : Balloon option;
    mutable R : Balloon option;
}

let popEm balloons =
    let convert balloons = seq {
        let mutable previous = None
        for b in balloons do
            let current = Some({ L = None ; Value = b ; R = None})
            if previous.IsSome then
                previous.Value.R <- current
                current.Value.L <- previous
                
                yield previous
                previous <- current

        if previous.IsSome then yield previous
    }

    let rec flatten b total =
        match b with
        | { L = Some(l) ; R = Some(r) } when l.Value > b.Value && b.Value < r.Value ->
            l.R <- Some(r)
            r.L <- Some(l)

            flatten l (total + l.Value * b.Value * r.Value)

        | { R = Some(r) } -> flatten r total
        | _ -> total

    let rec findPeak b =
        match b with
        | { R = Some(r) } -> if b.Value > r.Value then b else findPeak r
        | _ -> b

    let rec rollUpLeft b total =
        match b with
        | { L = Some(l1) } & { L = Some({ L = Some(l2) }) } ->
            b.L <- Some(l2)
            l2.R <- Some(b)

            rollUpLeft b (total + l2.Value * l1.Value * b.Value)
        | _ -> total

    let rec rollUpRight b total =
        match b with
        | { R = Some(r1) } & { R = Some({ R = Some(r2) }) } ->
            b.R <- Some(r2)
            r2.L <- Some(b)

            rollUpRight b (total + b.Value * r1.Value * r2.Value)
        | _ -> total
    
    let finalize b total =
        let possibleTotals =
            match b with
            | { L = Some(l) } & { R = Some(r) } ->
                [
                    l.Value * b.Value           + b.Value * r.Value + b.Value
                    l.Value * b.Value * r.Value + l.Value * r.Value + l.Value
                    l.Value * b.Value * r.Value + l.Value * r.Value + r.Value
                ]

            | { L = Some(l) } -> 
                [
                    l.Value * b.Value + l.Value
                    l.Value * b.Value + b.Value
                ]

            | { R = Some(r) } ->
                [
                    r.Value * b.Value + r.Value
                    r.Value * b.Value + b.Value
                ]
            | _ -> [ b.Value ]
            
        total + (List.max possibleTotals)

    match convert balloons |> List.ofSeq with
    | Some(bl) :: r ->
        let totalFlat = flatten bl 0

        let peak = findPeak bl
        let totalLeft = rollUpLeft peak totalFlat
        let totalRight = rollUpRight peak totalLeft

        finalize peak totalRight

    | _ -> 0