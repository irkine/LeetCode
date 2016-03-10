module Test.CoinChange

open CoinChange
open Test.Common
open Xunit
open Swensen.Unquote

let convert str =
    str
    |> split ' '
    |> Seq.map System.Int32.Parse
    |> List.ofSeq

[<Theory>]
[<InlineData( "1 2 5"  , 12  , "2 5 5"                         )>]
[<InlineData( "2 5 12" , 11  , "2 2 2 5"                       )>]
[<InlineData( "1 3 4"  , 10  , "1 1 4 4"                       )>]
[<InlineData( "2 3 5"  , 14  , "2 2 5 5"                       )>]
[<InlineData( "10"     , 100 , "10 10 10 10 10 10 10 10 10 10" )>]
let mainline (coins, amount, expectedChange) =

    let input = convert coins
    let expected = convert expectedChange

    test <@ expected = MakeChange input amount @>

[<Theory>]
[<InlineData( "2 5" , 3   )>]
[<InlineData( ""    , 100 )>]
[<InlineData( "100" , 10  )>]
let negative (coins, amount) = mainline (coins, amount, "")