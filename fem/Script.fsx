// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#I @"C:\Users\bartolon\Documents\Visual Studio 2010\Projects\fem\distr\bin\Release"
#I @"C:\Users\Davide\Desktop\Projects\fem\fem\bin\Debug"
#I @"C:\progetti\fem\distr\bin\Debug"

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

(*
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

*)


// --------------------- esempio

//1) i valori misurati hanno un errore del 10%
//2) i tempi misurati hanno un errore del 0.01s
//3) le misure sono fatte ogni circa 1 secondi
//4) il valore misurato ha una persistenza di 3s
//5) il tempo di inizio e di fine ha una precisione di 0.1 s
//6) tutte gli errori supponiamo di forma gaussiana

let step a b p = dist {
    let! s = uniform
    return if s<p then a else b
    }

(*let rec distance n = 
    if n = 0 then
        Random.always 0,Random.always 0
    else
        let lastStep,maxVal = distance (n - 1)
        let newStep = Dist.toRandom(step -1 1 0.5)
        let newVal = lastStep .+ newStep
        //let a = RandomVariable.lift2 (max)
        (newVal, RandomVariable.lift2 max newVal maxVal)
        *)

let rec supera stepnumber value =
    if value <= 0 then
        constDist 1.
    else if stepnumber = 0 then
        constDist 0.
    else
        dist {
            let! newStep = step -1 1 0.5
            return! supera (stepnumber - 1) (value - newStep)
            }

        //dist.Bind(step -1 1 0.5,fun newStep -> dist.Bind(supera (stepnumber - 1) (value - newStep),fun recurse -> dist.Return(recurse)))
//probabilita' che dopo n step abbiamo superato un valore N


let rec probab wastable distance =
    let p = 0.5
    if distance <= 0 then
        1.0
    else if wastable <= 0 then
        0.0
    else
        (1.0 - p) * probab (wastable - 1) (distance + 1) + p * probab (wastable - 1) (distance - 1)
        

let initialTime = 0.
let N = 50000

let nums1 = [| 0.0 ; 18. ; 19. ; 15. ; 19. ; 25. ; 33. ; 31. ; 36.; 32. ;  0.0 |]
let nums2 = [| 0.0 ; 57. ; 67. ; 69. ; 56. ; 0.0 |]

let measures (nums:float[]) =  Seq.toList (seq{
    yield (Random.always 0.) , Dist.toRandom (gaussianBoxMuller initialTime 0.1)
    let l = nums.Length - 1
    for i in 0..l do
        let percError = (Dist.toRandom (gaussianBoxMuller 1. 0.01))
        let value,time = float(nums.[i]),float(i)
        yield (Random.always value) .* percError, Dist.toRandom (gaussianBoxMuller time 0.01)
     })

let integrated l = Seq.zip l (Seq.skip 1 l) |> Seq.map (fun ((_,t1),(v,t2)) -> v .*(t2 .- t1)) |> Seq.fold (.+) (Random.always 0.)
let integral1 = integrated (measures nums1)
let integral2 = integrated (measures nums2)

let diff = integral2 .- integral1
let perc = diff ./ integral2 .* (Random.always 100.)

integral1.Samples |> Seq.take N  |> bucket 1. |> FSharpChart.Line |> FSharpChart.Create 
let avg1 = integral1.Samples |> Seq.take N |> Seq.average
let stdDev1 = System.Math.Sqrt ((integral1.Samples |> Seq.take N |> Seq.fold (fun (s:float) (x:float) -> s + (x-avg1)*(x-avg1) ) (0.) ) / float(N) )

integral2.Samples |> Seq.take N  |> bucket 1. |> FSharpChart.Line |> FSharpChart.Create 
let avg2 = integral2.Samples |> Seq.take N |> Seq.average
let stdDev2 = System.Math.Sqrt ((integral2.Samples |> Seq.take N |> Seq.fold (fun (s:float) (x:float) -> s + (x-avg2)*(x-avg2) ) (0.) ) / float(N) )

[| integral1.Samples |> Seq.take N  |> bucket 1. ; integral2.Samples |> Seq.take N  |> bucket 1. |] |> Seq.map (fun x -> FSharpChart.Line x :> ChartTypes.GenericChart) |> FSharpChart.Combine |> FSharpChart.Create

perc.Samples |> Seq.take N  |> bucket 10. |> FSharpChart.Line |> FSharpChart.Create 
let avg3 = perc.Samples |> Seq.take N |> Seq.average
let stdDev3 = System.Math.Sqrt ((perc.Samples |> Seq.take N |> Seq.fold (fun (s:float) (x:float) -> s + (x-avg3)*(x-avg3) ) (0.) ) / float(N) )


[| nums1 ; nums2 |] |> Seq.map (fun x -> FSharpChart.Line x :> ChartTypes.GenericChart) |> FSharpChart.Combine |> FSharpChart.Create

let J1 = (Seq.average nums1) * float(nums1.Length)
let J2 = (Seq.average nums2) * float(nums2.Length)


let m = Dist.toRandom (gaussianBoxMuller 1. 0.1) .+ (Random.always 10.)
m.Samples |> Seq.take N  |> bucket 100. |> FSharpChart.Line |> FSharpChart.Create 


