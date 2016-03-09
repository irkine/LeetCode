module Test.Common

open System

let split (c : char) (str : string) = str.Split([| c |], StringSplitOptions.RemoveEmptyEntries)