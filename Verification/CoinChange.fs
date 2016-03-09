module Test.CoinChange

open Test.Common
open CoinChange
open Xunit

let convert str =
    str
    |> split ' '
    |> Seq.map System.Int32.Parse
    |> List.ofSeq

[<Theory>]
[<InlineData( "1 2 5"  , 12  , "5 5 2"                         )>]
[<InlineData( "2 5 12" , 11  , "5 2 2 2"                       )>]
[<InlineData( "1 3 4"  , 10  , "4 4 1 1"                       )>]
[<InlineData( "2 3 5"  , 14  , "5 5 2 2"                       )>]
[<InlineData( "10"     , 100 , "10 10 10 10 10 10 10 10 10 10" )>]
let mainline (coins, amount, expectedChange) =

    let input = convert coins
    let expected = convert expectedChange

    let actual = MakeChange input amount
    Assert.Equal<int[]>(
        expected |> Array.ofList,
        actual |> List.rev |> Array.ofList)

[<Theory>]
[<InlineData( "2 5" , 3   )>]
[<InlineData( ""    , 100 )>]
[<InlineData( "100" , 10  )>]
let negative (coins, amount) = mainline (coins, amount, "")