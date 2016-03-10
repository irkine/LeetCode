module Test.HouseRobber

open HouseRobber
open Test.Common
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("0 1 2 3 4 5 6", 12)>]
[<InlineData("6 5 4 3 2 1 0", 12)>]
[<InlineData("1 10 1000 100 100 1000", 2001)>]
[<InlineData("1 10 1 100 1000 10", 1010)>]
let mainline (houses, expectedTotal) =
    let input =
        houses
        |> split ' '
        |> Seq.map System.Int32.Parse
        |> Seq.toList

    test <@ expectedTotal = rob input @>

[<Fact>]
let ``time complexity`` () =
    let randomList =
        let rnd = System.Random()
        fun count -> List.init count (fun i -> rnd.Next(1000))

    let time count =
        let input = randomList count
        let w = System.Diagnostics.Stopwatch.StartNew()
        rob input |> ignore
        w.Stop()

        w.Elapsed.TotalMilliseconds

    let t1 = time 10
    let t2 = time 100
    let t3 = time 1000
    let t4 = time 1000000

    test <@ t2 <= t1 * 15.0   @>
    test <@ t3 <= t2 * 15.0   @>
    test <@ t4 <= t3 * 1500.0 @>