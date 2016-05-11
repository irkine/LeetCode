module BurstBalloons // 312. Burst Balloons

open System

type Balloon = {
    Value : int;
    mutable L : Balloon option;
    mutable R : Balloon option;
}

let popEm balloons =
    let rec repeat fxn count b =
         match fxn b with
         | Some(n) when count > 0 -> repeat fxn (count-1) n
         | _ when count > 0 -> None
         | _ -> Some(b)

    let rec movl = repeat (fun b -> b.L)
    let rec movr = repeat (fun b -> b.R)

    let convert balloons =
        let rec rlink balloons =
            match balloons with
            | b :: r -> Some({ L = None ; Value = b ; R = rlink r })
            | _ -> None
        
        let llink cursor =
            let rec loop cursor head peak =
                match cursor.R with
                | Some(n) ->
                    n.L <- Some(cursor)
                    let peak' = Array.maxBy (fun b -> b.Value) [| peak ; n |]
                    loop n head peak'

                | _ -> (head, peak)

            match cursor with
            | Some(c) -> Some(loop c c c)
            | None -> None

        balloons |> rlink |> llink

    let handleRun b findMiddle =
        let rec loop b v e1 getNext getPrev count =
            match (e1, getNext 1 b) with
            
            // Walk
            | ( _ , Some(n) ) when n.Value = v ->
                loop n v e1 getNext getPrev (count+1)

            // Next Edge Greater
            | ( Some(e), Some(n) ) when e.Value < n.Value ->
                if findMiddle
                then getPrev ((count - 1 + count % 2)/2) b
                else getPrev 1 n

            // Next Edge Only
            | ( None, Some(n) ) ->
                if findMiddle
                then getPrev ((count - 1 + count % 2)/2) b
                else getPrev 1 n

            // Previous Edge Only, Greater; or no Edges
            | _ ->
                if findMiddle
                then getPrev (count / 2) b
                else getPrev (count - 1) b
        
        match b with
        | { L = Some(l) } when l.Value = b.Value -> loop b b.Value b.R movl movr 1
        | { R = Some(r) } when r.Value = b.Value -> loop b b.Value b.L movr movl 1
        | _ -> Some(b)
        
    let rec flatten b (p, total) =
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

                let next = if l.Value < r.Value then l else r
                consumeMinima next (total + l.Value * b.Value * r.Value)

            | _ -> (b, total)

        match findMinima b with
        | Some(m) ->
            let (b', total') = consumeMinima m total
            flatten b' (p, total')

        | _ -> (p, total)

    let consumePeak (peak, total) =
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

            | _ -> (b, total)

        let b = (handleRun peak true).Value

        let totalLeft  = totalRun b (movl 1)
        let totalRight = totalRun b (movr 1)

        loop b totalLeft totalRight total

    let rec rollUpLeft (p, total) =
        match p with
        | { L = Some(l1) } & { L = Some({ L = Some(l2) }) } ->
            p.L <- Some(l2)
            l2.R <- Some(p)

            rollUpLeft (p, (total + l2.Value * l1.Value * p.Value))
        | _ -> (p, total)

    let rec rollUpRight (p, total) =
        match p with
        | { R = Some(r1) } & { R = Some({ R = Some(r2) }) } ->
            p.R <- Some(r2)
            r2.L <- Some(p)

            rollUpRight (p, (total + p.Value * r1.Value * r2.Value))
        | _ -> (p, total)

    let finalize (b, total) =
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

    let compute b =
        flatten b
        >> consumePeak
        >> rollUpLeft
        >> rollUpRight
        >> finalize

    match convert balloons with
    | Some((b, p)) -> compute b (p, 0)
    | None -> 0