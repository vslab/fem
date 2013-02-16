module Main

open Distributions
open RandomVariables

let d1 = dist { return true }
let d2 = dist { return 1 }
(*let r1 = random {
    let! a = d1
    let! b = d2
    return if a then b else 2
    }*)

let f2 a b =
    let value = if a then b else 2
    random.Return(value)
let f1 a =
    random.Bind(d2,f2 a)
(*
let r1 = random.Bind(d1, f1)
*)
let r3 = dist { return true }
let r4 = dist { return 1 }
let r2 = random { let! a = r3 in if a then let! b = r4 in return 1 else return 0 }
let d = getDist r2
let g = gen()

[<EntryPoint>]
let main args =
    let v = d.Value(g)
    printfn ("%d") (v)
    System.Console.WriteLine  (v)
    System.Diagnostics.Debug.WriteLine (v)
    v

