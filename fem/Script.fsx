// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "Uniform.fs"

#load "Module1.fs"
open Module1

#r @"MSDN.FSharpChart.dll"
#r @"System.Windows.Forms.DataVisualization.dll"
open MSDN.FSharp.Charting

// example with good seeds
let x1 = Uniform.getSource(Uniform.fresh())
let x2 = Uniform.getSource(Uniform.fresh())
let x3 = Uniform.getSource(Uniform.fresh())
let x4 = Uniform.getSource(Uniform.fresh())
let x5 = Uniform.getSource(Uniform.fresh())
let x6 = Uniform.getSource(Uniform.fresh())
let x7 = Uniform.getSource(Uniform.fresh())
let x8 = Uniform.getSource(Uniform.fresh())
let sources = [x1; x2; x3; x4; x5; x6; x7; x8]


let advanceAll (sList:samplerEnumerator list) =
    sList |> List.iter (fun s -> Uniform.moveNext(s))

let printAll (sList:samplerEnumerator list) =
    sList |> List.iter (fun s -> printf "\n%f" s.Current)

printAll sources
advanceAll sources

let makeSeq (sList:samplerEnumerator list) f =
    seq {
        while (true) do
            advanceAll sList        
            yield f sList
    }

let foo1 (sList:samplerEnumerator list) =
  (gaussianBoxMuller 0. 1. sList.[0] sList.[1]) + (gaussianBoxMuller 0. 1. sList.[2] sList.[3]) +
  (gaussianBoxMuller 0. 1. sList.[4] sList.[5]) + (gaussianBoxMuller 0. 1. sList.[6] sList.[7])

let foo2 (sList:samplerEnumerator list) =
  (gaussianBoxMuller 0. 1. sList.[0] sList.[1]) +   (gaussianBoxMuller 0. 1. sList.[0] sList.[1]) +
  (gaussianBoxMuller 0. 1. sList.[0] sList.[1]) +   (gaussianBoxMuller 0. 1. sList.[0] sList.[1])

FSharpChart.Rows [
  makeSeq sources foo1  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column ;
  makeSeq sources foo2  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column
 ] |> FSharpChart.Create

// http://en.wikibooks.org/wiki/F_Sharp_Programming/Computation_Expressions

