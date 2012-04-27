// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#r @"C:\Users\bartolon\Documents\Visual Studio 2010\Projects\fem\distr\bin\Release\RandomTools.dll"
#r @"C:\Users\bartolon\Documents\Visual Studio 2010\Projects\fem\distr\bin\Release\distr.dll"

open Distributions
open RandomTools
open RandomVariables

#r @"MSDN.FSharpChart.dll"
#r @"System.Windows.Forms.DataVisualization.dll"
open MSDN.FSharp.Charting

open System.Windows.Forms.DataVisualization

let bucket (x:float) (y:seq<float>) =
      y |> Seq.countBy(fun v -> floor(v*x)/x) |> Seq.toList |> Seq.sortBy (fun (x,_)-> x )

//let covariance (x:seq<float>) (y:seq<float>) = 

//seq { while true do yield ss |> binomial 0.5 20 } |> Seq.take 1000000 |> Seq.countBy (fun v -> v) |> Seq.toList |> List.sort |> FSharpChart.Line |> FSharpChart.Create
let SystematicError = Dist.toRandom (gaussianBoxMuller 0. 1.)
let measures =  Seq.toList (seq {
        for i = 1 to 100 do yield Dist.toRandom (gaussianBoxMuller (double(i)) 0.5)
    })

let mysum = Seq.fold (fun s v -> s .+ v) (Random.always 0.) measures
let myavg = mysum ./ (Random.always 100.)
myavg


//(measures |> Seq.fold (fun s v -> s + v) (Dist.toRandom (dist { return 0. }))) (Dist.toRandom (dist { return 100. })))


//pippo
//Seq.zip pippo2 pippo |> Seq.map (fun (x,y) -> x+y )
myavg.Samples |> Seq.take 100000  |> bucket 100. |> FSharpChart.Line |> FSharpChart.Create

