// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "Uniform.fs"

#load "Module1.fs"
open Module1

#r @"MSDN.FSharpChart.dll"
#r @"System.Windows.Forms.DataVisualization.dll"
open MSDN.FSharp.Charting


let s n = Uniform.givemeSampler n

let alone () =
  (gaussianBoxMuller 0. 1. (s "1a") (s "1b")) 

let a() = gaussianBoxMuller 0. 1. (s "1a") (s "1b")
let b() = gaussianBoxMuller 0. 1. (s "1a") (s "1b")
let c() = gaussianBoxMuller 0. 1. (s "1a") (s "1b")


let independent () =
  (gaussianBoxMuller 0. 1. (s "1a") (s "1b")) + (gaussianBoxMuller 0. 1. (s "2a") (s "2b")) //+
  //(gaussianBoxMuller 0. 1. (s "3a") (s "3b")) + (gaussianBoxMuller 0. 1. (s "4a") (s "4b"))

let dependent () =
  (gaussianBoxMuller 0. 1. (s "1a") (s "1b")) +   (gaussianBoxMuller 0. 1. (s "1a") (s "1b")) //+
  //(gaussianBoxMuller 0. 1. (s "1a") (s "1b")) +   (gaussianBoxMuller 0. 1. (s "1a") (s "1b"))

FSharpChart.Rows [
  Uniform.makeSeq (a+b+c)  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column ;
  Uniform.makeSeq independent  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column ;
  Uniform.makeSeq dependent  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column
 ] |> FSharpChart.Create

let f() = a()+b()+c()

let mul () =
   (gaussianBoxMuller 0. 1. (s "1a") (s "1b"))

let mul2 () =
   (gaussianBoxMuller 0. 1. (s "1a") (s "1b")) * 2.

   fn : float -> float -> float -> float
   
   seq {
     let x = a + b
     }

dist {
let x = fn a b c
}
let f1() =
    a1=a()


FSharpChart.Rows [
  Uniform.makeSeq f  |> Seq.take 100000 |> bucket 5. |> FSharpChart.Column ;
 ] |> FSharpChart.Create

// http://en.wikibooks.org/wiki/F_Sharp_Programming/Computation_Expressions

