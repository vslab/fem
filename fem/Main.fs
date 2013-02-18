module Main

open Distributions
open RandomVariables

//open RandomTools
//let s = RandomSource()
let g = Distributions.gen() 


[<EntryPoint>]
let main args =
    let v = 1
    printfn ("%d") (v)
    System.Console.WriteLine  (v)
    System.Diagnostics.Debug.WriteLine (v)
    v

