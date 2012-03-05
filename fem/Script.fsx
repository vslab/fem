// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "Uniform.fs"

#load "Module1.fs"
open Module1

#r @"MSDN.FSharpChart.dll"
#r @"System.Windows.Forms.DataVisualization.dll"
open MSDN.FSharp.Charting

open System.Windows.Forms.DataVisualization


seq { while true do yield ss |> binomial 0.5 20 } |> Seq.take 1000000 |> Seq.countBy (fun v -> v) |> Seq.toList |> List.sort |> FSharpChart.Line |> FSharpChart.Create

let foo1 x =
  (gaussianBoxMuller 0. 1. x) + (gaussianBoxMuller 1. 1. x)

FSharpChart.Rows [
  sample foo1  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column
 ] |> FSharpChart.Create

// http://en.wikibooks.org/wiki/F_Sharp_Programming/Computation_Expressions

type Sum() =
    member o.Bind(v,f) = f(v)
    member o.ReturnFrom(v) = v

let sum = Sum()

let m1 =
    sum {
        //let! x = 1.
        return!  1.
    } 

let m2 = 
    sum {
        let! v1 = m1
        return! v1 + 1.
    }

