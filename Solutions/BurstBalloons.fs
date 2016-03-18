module BurstBalloons // 312. Burst Balloons

open System

type Balloon = {
    Value : int;
    mutable L : Balloon option;
    mutable R : Balloon option;
}

let popEm balloons =
    let rec movl count b =
        match b with
        | { L = Some(l) } when count > 0-> movl (count-1) l
        | _ when count > 0 -> None
        | _ -> Some(b)

    let rec movr count b =
        match b with
        | { R = Some(r) } when count > 0 -> movr (count-1) r
        | _ when count > 0 -> None
        | _ -> Some(b)

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

    let handleRun b findMiddle =
        let rec loop b v e1 getNext getPrev count =
            match (e1, getNext 1 b) with
            
            // Walk
            | ( _ , Some(n) ) when n.Value = v ->
                loop n v e1 getNext getPrev (count+1)

            // Left Greater
            | ( Some(e), Some(n) ) when e.Value < n.Value ->
                if findMiddle
                then getPrev ((count - 1 + count % 2)/2) b
                else getPrev 1 n

            // Left Only
            | ( None, Some(n) ) when count % 2 = 0 ->
                if findMiddle
                then getPrev ((count - 1 + count % 2)/2) b
                else getPrev 1 n

            // Right Only or Greater
            | _ ->
                if findMiddle
                then getPrev (count / 2) b
                else getPrev (count - 1) b

        match b with
        | { L = Some(l) } when l.Value = b.Value -> loop b b.Value b.R movl movr 1
        | { R = Some(r) } when r.Value = b.Value -> loop b b.Value b.L movr movl 1
        | _ -> Some(b)
        
    let rec flatten b total = // Need to manage state-change, dec -> inc, not rely on immediate left/right ( 2 1 1 2 )
        let findMinima b =
            let rec loop b desc = 
                match b with
                | { R = Some(r) } ->
                    if b.Value < r.Value && desc then handleRun b false
                    else if b.Value > r.Value then loop r true
                    else loop r desc
                | _ -> None

            loop b false

        let rec consumeMinima b total =
            match b with
            | { L = Some(l) ; R = Some(r) } when l >= b && b <= r ->
                l.R <- Some(r)
                r.L <- Some(l)

                let next = 
                    if l.Value < r.Value then l
                    else r
                
                consumeMinima next (total + l.Value * b.Value * r.Value)

            | _ -> (b, total)

        match findMinima b with
        | Some(m) ->
            let (bl, tl) = consumeMinima m total
            flatten bl tl
            
        | _ -> total

    let findPeak b =
        let rec loop b =
            match b with
            | { R = Some(r) } -> if b.Value > r.Value then b else loop r
            | _ -> b

        (handleRun (loop b) true).Value

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
    | Some(b) ->
        let totalFlat = flatten b 0

        let (totalConsume, peak) = consumePeak (findPeak b) totalFlat
        let totalLeft = rollUpLeft peak totalConsume
        let totalRight = rollUpRight peak totalLeft

        finalize peak totalRight

    | None -> 0