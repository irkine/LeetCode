module Balloons.Test

open Balloons
open Xunit

[<Theory>]
[<InlineData("3,1,5,8"       , 167)>]
[<InlineData("8"             , 8)>]
[<InlineData("2,8"           , 24)>]
[<InlineData("1,2,3,4,5,6"   , 252)>]
[<InlineData("6,5,4,3,2,1"   , 252)>]
[<InlineData("3,2,1,2,3"     , 46)>]
[<InlineData("3,2,5,2,3"     , 117)>]
[<InlineData("2,3,2,1,2,3,2" , 70)>]
[<InlineData("5,2,2,2"       , 55)>]
[<InlineData("2,2,2,5"       , 55)>]
let pop (balloons : string) expected =
    let b =
        balloons.Split([|','|])
        |> Array.map System.Int32.Parse
        |> Array.toList

    let actual = popEm b

    Assert.Equal(expected, actual)
