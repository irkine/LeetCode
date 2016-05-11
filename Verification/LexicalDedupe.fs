module Test.LexicalDedupe

open LexicalDedupe
open Test.Common
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("bcabc", "abc")>]
[<InlineData("cbacdcbc", "acdb")>]
let mainline (letters : string) (expected : string) =
    let l = letters |> Seq.toList
    let e = expected |> Seq.toList

    test <@ e = lexicalRemoveDedupe l @>