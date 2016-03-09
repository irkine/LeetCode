﻿module Test.Itenerary

open Test.Common
open Itenerary
open Xunit

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

    let actual = reconstruct "JFK" t

    Assert.Equal<string list>(e, actual)