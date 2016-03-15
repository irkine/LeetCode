module BurstBalloons // 312. Burst Balloons

type Balloon = {
    Value : int;
    mutable L : Balloon option;
    mutable R : Balloon option;
}

let popEm balloons =
    let convert balloons =
        let rec rlink balloons =
            match balloons with
            | b :: r -> Some({ L = None ; Value = b ; R = rlink r })
            | _ -> None
        
        let llink cursor =
            let rec loop cursor head =
                match cursor.R with
                | Some(n) ->
                    n.L <- Some(cursor)
                    loop n head
                | _ -> head

            match cursor with
            | Some(c) -> Some(loop c c)
            | None -> None

        balloons |> rlink |> llink

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

    let consumePeak b total =
        let totalRun b getNext =
            let rec loop b getNext total =
                match getNext b with
                | Some(n) -> loop n getNext (total + n.Value)
                | None -> total
            
            loop b getNext 0

        let rec loop b totalLeft totalRight total =
            match b with
            | { L = Some(l) ; R = Some(r) } when totalLeft + totalRight <= l.Value * r.Value ->
                let newTotal = total + l.Value * b.Value * r.Value

                l.R <- Some(r)
                r.L <- Some(l)

                if l.Value > r.Value
                then loop l (totalLeft - l.Value) totalRight             newTotal
                else loop r totalLeft             (totalRight - r.Value) newTotal

            | _ -> (total, b)

        let totalLeft  = totalRun b (fun b -> b.L)
        let totalRight = totalRun b (fun b -> b.R)

        loop b totalLeft totalRight total

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
                [|
                    l.Value * b.Value           + b.Value * r.Value + b.Value
                    l.Value * b.Value * r.Value + l.Value * r.Value + l.Value
                    l.Value * b.Value * r.Value + l.Value * r.Value + r.Value
                |]

            | { L = Some(l) } -> 
                [|
                    l.Value * b.Value + l.Value
                    l.Value * b.Value + b.Value
                |]

            | { R = Some(r) } ->
                [|
                    r.Value * b.Value + r.Value
                    r.Value * b.Value + b.Value
                |]

            | _ -> [| b.Value |]
            
        total + (Array.max possibleTotals)

    match convert balloons with
    | Some(bl) ->
        let totalFlat = flatten bl 0

        let (totalConsume, peak) = consumePeak (findPeak bl) totalFlat
        let totalLeft = rollUpLeft peak totalConsume
        let totalRight = rollUpRight peak totalLeft

        finalize peak totalRight

    | None -> 0