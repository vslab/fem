// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#I @"C:\Users\bartolon\Documents\Visual Studio 2010\Projects\fem\distr\bin\Release"
#I @"C:\Users\Davide\Desktop\Projects\fem\fem\bin\Debug"

#r @"RandomTools.dll"
#r @"distr.dll"

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



//1) i valori misurati hanno un errore del 5%
//2) i tempi misurati hanno un errore del 0.001s
//3) le misure sono fatte ogni circa 1 secondi
//4) il valore misurato ha una persistenza di 3s
//5) il tempo di inizio e di fine ha una precisione di 0.1 s
//6) tutte gli errori supponiamo di forma gaussiana

let tempoIniziale = 0.

let nums1 = [| 2.0 ; 3.0 ; 4.0 ; 4.0 ; 5.0 ; 5.0 ; 6.0 ; 3.0; 2.0 ; 2.0 |]
let nums2 = [| 2.0 ; 4.0 ; 5.0 ; 6.0 ; 5.0 ; 2.0 ; 6.0 ; 5.0; 4.0 ; 2.0 |]

let misure (nums:float[]) =  Seq.toList (seq{
    yield (Random.always 0.) , Dist.toRandom (gaussianBoxMuller tempoIniziale 0.1)
    for i in 0..9 do
        let errPercentuale = (Dist.toRandom (gaussianBoxMuller 1. 0.05))
        let valore,tempo = float(nums.[i]),float(i)
        yield (Random.always valore) .* errPercentuale, Dist.toRandom (gaussianBoxMuller tempo 0.001)
     })


let integrated l = Seq.zip l (Seq.skip 1 l) |> Seq.map (fun ((_,t1),(v,t2)) -> v .*(t2 .- t1)) |> Seq.fold (.+) (Random.always 0.)
let integral1 = integrated (misure nums1)
let integral2 = integrated (misure nums2)

let N = 50000

integral1.Samples |> Seq.take N  |> bucket 100. |> FSharpChart.Line |> FSharpChart.Create 
let avg1 = integral1.Samples |> Seq.take N |> Seq.average
let stdDev1 = System.Math.Sqrt ((integral1.Samples |> Seq.take N |> Seq.fold (fun (s:float) (x:float) -> s + (x-avg)*(x-avg) ) (0.) ) / float(N) )

integral2.Samples |> Seq.take N  |> bucket 100. |> FSharpChart.Line |> FSharpChart.Create 
let avg2 = integral2.Samples |> Seq.take N |> Seq.average
let stdDev2 = System.Math.Sqrt ((integral2.Samples |> Seq.take N |> Seq.fold (fun (s:float) (x:float) -> s + (x-avg)*(x-avg) ) (0.) ) / float(N) )


