// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "Uniform.fs"

#load "Module1.fs"
open Module1

#r @"MSDN.FSharpChart.dll"
#r @"System.Windows.Forms.DataVisualization.dll"
open MSDN.FSharp.Charting



let independent () =
  let s1 = Uniform.givemeSampler "gaussian1m"
  let s2 = Uniform.givemeSampler "gaussian1s"
  let s3 = Uniform.givemeSampler "gaussian2m"
  let s4 = Uniform.givemeSampler "gaussian2s"
  let s5 = Uniform.givemeSampler "gaussian3m"
  let s6 = Uniform.givemeSampler "gaussian3s"
  let s7 = Uniform.givemeSampler "gaussian4m"
  let s8 = Uniform.givemeSampler "gaussian4s"
  (gaussianBoxMuller 0. 1. s1 s2) + (gaussianBoxMuller 0. 1. s3 s4) +
  (gaussianBoxMuller 0. 1. s5 s6) + (gaussianBoxMuller 0. 1. s7 s8)

let dependent () =
  let s1 = Uniform.givemeSampler "gaussian1m"
  let s2 = Uniform.givemeSampler "gaussian1s"
  (gaussianBoxMuller 0. 1. s1 s2) +   (gaussianBoxMuller 0. 1. s1 s2) +
  (gaussianBoxMuller 0. 1. s1 s2) +   (gaussianBoxMuller 0. 1. s1 s2)

FSharpChart.Rows [
  Uniform.makeSeq independent  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column ;
  Uniform.makeSeq dependent  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column
 ] |> FSharpChart.Create

// http://en.wikibooks.org/wiki/F_Sharp_Programming/Computation_Expressions

