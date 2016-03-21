﻿module Test.BurstBalloons

open BurstBalloons
open Test.Common
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData( "8"             , 8    )>]
[<InlineData( "2 8"           , 24   )>]
[<InlineData( "8 2"           , 24   )>]
[<InlineData( "2 2 5"         , 35   )>]
[<InlineData( "5 2 2"         , 35   )>]
[<InlineData( "2 6 2"         , 30   )>]
[<InlineData( "2 5 2"         , 26   )>]
[<InlineData( "3 1 5 8"       , 167  )>]
[<InlineData( "3 2 1 2 3"     , 46   )>]
[<InlineData( "3 2 5 2 3"     , 117  )>]
[<InlineData( "1 2 3 4 5 6"   , 252  )>]
[<InlineData( "6 5 4 3 2 1"   , 252  )>]
[<InlineData( "9 2 14 5 3"    , 1053 )>]
[<InlineData( "2 3 2 1 2 3 2" , 70   )>]
[<InlineData( "2 5 5 5 2"     , 201  )>]
[<InlineData( "2 5 5 3"       , 114  )>]
[<InlineData( "3 5 5 2"       , 114  )>]
[<InlineData( "3 5 4 2"       , 93   )>]
[<InlineData( "2 1 1 1 2"     , 14   )>]
[<InlineData( "2 1 1 1 2"     , 14   )>]
[<InlineData( "3 1 1 2"       , 18   )>]
[<InlineData( "2 1 1 3"       , 18   )>]
[<InlineData( "2 3 9 11 7 15 5 11 5 14 3 3 3 13 6 2 14 13 3 9 4 8 11 5 13 10 12 2 11 13 13 2 10 4 8 7 1 10 1 5 5 13 9 9 8 2 9 5 11 6 3 12 12 12 4 15 2 10 5 3 14 5 14 2 4 2 13 14 9 2 14 12 9 13 5 7 10 5 6 11 9 5 12 13 4 6 11 6 3 11 13 2 6 12 6 15 10 15 3 14 10 15 2 14 10 5 11 8 2 8 6 7 12 2 5 9 15 5 14 10 13 9 14 11 6 15 5 9 5 2 2 3 4 14 7 1 1 8 3 8 5 3 8 10 2 3 4 6 12 15 12 11 6 12 14 7 12 12 13 5 8 4 7 4 4 9 1 9 5 12 13 5 11 11 14 2 1 7 12 14 3 9 2 7 9 10 8 12 12 1 6 4 14 7 7 2 1 1 10 3 2 5 7 2 12 15 10 1 4 6 10 7 5 12 2 13 10 11 13 14 7 12 15 8 12 7 7 2 11 12 13 4 3 10 10 8 9 8 7 4 15 2 6 14 10 3 2 15 10 6 6 12 13 15 5 1 9 3 4 9 12 12 13 4 13 12 3 7 2 3 13 2 3 5 7 14 12 4 7 12 9 10 7 7 5 14 10 3 11 9 6 12 4 4 8 2 9 13 13 6 11 12 2 1 14 10 2 5 10 13 12 8 9 9 7 5 9 11 12 5 13 9 12 3 1 2 13 4 3 3 9 8 15 4 6 4 7 5 11 11 11 7 12 14 12 8 6 9 9 7 10 3 3 1 11 3 3 3 11 5 9 13 12 3 8 15 9 14 13 11 6 14 14 7 4 9 8 1 4 9 4 6 2 11 11 8 14 4 9 6 5 7 14 3 6 12 6 13 11 12 9 11 12 14 11 2 12 8 7 9 3 7 2 3 2 14 8 5 3 3 13 9 9 6 8 3 1 7 3 14 15 11 2 3 5 1 14 11 6 3 11 5 13 13 8 10 3 11 10 7 3 6 10 12 2 2 2 1 15 14 15 9 9 8 4 11 6 3 2 4 14 2 3 1 1 11 5 1 3 10 13 15 1 6 11 4 4 2 14 3 11 14 7 10 9 6 13 3 4 4 12 12 14 4 12 2 10 4 6 14 9 14 12 12 3 7 3 12 14 12 8 2 12 13 3 7 8 9 13 8 3 7 2 5 11 11 12 14 9 2 14 9 10 6 11 11 12 6 3 6 15 13 3 5 11 6 9 3 3 2 7 11 4 9 13 6 15 13 14 8 2 11 1 6 5 9 7 13 5 11 11 4 9 11 10 12 5 14 9 3 5 14 6 3 14 2 14 9 7 10 4 13 9 5 9 8 2 14 3 3 2 11 10 1 9 7 8 11 7 14 11 7 10 3 7 6 10 9 14 4 12 15 13 5 14 9 2 11 13 3 14 5 7 9 11 6 6 5 8 11 7 2 14 12 5 10 10 8 1 8 13 9 14 10 6 12 13 14 14 2 9 6 12 9 9 5 5 1 2 2 6 9 4 8 4 14 7 9 2 2 1 3 5 14 5 10 9 2 11 2 7 2 12 13 13 9 10 3 2 11 7 8 10 4 2 6 1 2 5 15 5 4 2 15 3 12 10 14 6 9 3 2 6 15 3 8 13 4 14 5 6 10 14 13 6 11 3 13 3 11 7 6 8 1 14 7 4 9 13 2 2 8 4 12 6 6 9 14 5 15 15 7 6 4 10 3 13 5 9 12 8 11 3 5 11 2 5 10 3 6 15 7 12 1 4 6 9 12 8 2 8 13 4 2 9 12 11 3 13 8 1 11 2 2 3 14 11 8 10 8 5 10 12 1 1 12 13 15 13 9 5 13 12 8 2 13 12 10 11 9 8 7 14 8 7 9 4 12 7 4 7 4 13 15 6 8 3 8 2 8 8 3 14 11 8 9 8 5 1 4 5 13 7 7 10 7 6 2 3 11 15 2 10 14 7 14 8 13 6 12 10 12 7 1 5 13 10 5 2 1 12 8 3 15 6 2 15 13 14 9 12 14 3 1 9 6 13 1 4 5 7 15 12 3 3 8 10 2 5 14 5 6 3 6 7 12 10 14 13 9 14 3 14 6 14 8 3 3 8 13 10 2 6 12 2 14 4 12 8 10 8 12 5 13 6 9 6 7 1 12 8 1 3 11 13 5 7 7 2 2 1 13 12 6 12 6 2 9 14 9 3 7 5 10 8 1 15 4 10 9", 1301055)>]
let mainline (balloons : string) expected =
    let b =
        balloons
        |> split ' '
        |> Seq.map System.Int32.Parse
        |> Seq.toList

    test <@ expected = popEm b @>
