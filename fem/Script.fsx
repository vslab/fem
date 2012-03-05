// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "Uniform.fs"

#load "Module1.fs"
open Module1

#r @"MSDN.FSharpChart.dll"
#r @"System.Windows.Forms.DataVisualization.dll"
open MSDN.FSharp.Charting

let x1 = Uniform.getSource(1)
let x2 = Uniform.getSource(2)
let x3 = Uniform.getSource(3)
let x4 = Uniform.getSource(4)
let sources = [x1; x2; x3; x4]

let foo1 (sList:samplerEnumerator list) =
  (gaussianBoxMuller 0. 1. sList.[0] sList.[1]) + (gaussianBoxMuller 1. 1. sList.[2] sList.[3])

let advanceAll (sList:samplerEnumerator list) =
    sList |> List.iter (fun s -> Uniform.moveNext(s))

let makeSeq (sList:samplerEnumerator list) f =
    seq {
        while (true) do
            advanceAll sList        
            yield f sList
    }


FSharpChart.Rows [
  makeSeq sources foo1  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column
 ] |> FSharpChart.Create

// http://en.wikibooks.org/wiki/F_Sharp_Programming/Computation_Expressions

