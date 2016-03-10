module Test.Itenerary

open Itenerary
open Test.Common
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("MUC,LHR;JFK,MUC;SFO,SJC;LHR,SFO", "JFK,MUC,LHR,SFO,SJC")>]
[<InlineData("JFK,SFO;JFK,ATL;SFO,ATL;ATL,JFK;ATL,SFO", "JFK,ATL,JFK,SFO,ATL,SFO")>]
let mainline (tickets : string) (expected : string) =
    let t =
        tickets
        |> split ';'
        |> Seq.map (fun t ->
            let s = split ',' t
            (s.[0], s.[1]))
        |> Seq.toList

    let e =
        expected
        |> split ','
        |> Array.toList

    test <@ e = reconstruct "JFK" t @>