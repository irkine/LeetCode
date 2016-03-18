module Test.BurstBalloons

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
let mainline (balloons : string) expected =
    let b =
        balloons
        |> split ' '
        |> Seq.map System.Int32.Parse
        |> Seq.toList

    test <@ expected = popEm b @>
