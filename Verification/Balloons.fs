module Test.Balloons

open Balloons
open Test.Common
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData( "8"             , 8   )>]
[<InlineData( "2 8"           , 24  )>]
[<InlineData( "8 2"           , 24  )>]
[<InlineData( "2 2 5"         , 35  )>]
[<InlineData( "5 2 2"         , 35  )>]
[<InlineData( "2 6 2"         , 30  )>]
[<InlineData( "2 5 2"         , 26  )>]
[<InlineData( "3 1 5 8"       , 167 )>]
[<InlineData( "3 2 1 2 3"     , 46  )>]
[<InlineData( "3 2 5 2 3"     , 117 )>]
[<InlineData( "1 2 3 4 5 6"   , 252 )>]
[<InlineData( "6 5 4 3 2 1"   , 252 )>]
[<InlineData( "2 3 2 1 2 3 2" , 70  )>]
let mainline (balloons : string) expected =
    let b =
        balloons
        |> split ' '
        |> Seq.map System.Int32.Parse
        |> Seq.toList

    test <@ expected = popEm b @>
