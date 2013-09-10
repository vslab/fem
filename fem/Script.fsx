// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.






type Nat =
    | Zero
    | Succ of Nat

let isZero n =
    match n with
    | Zero -> true
    | Succ m -> false

let rec isEven n =
    match n with
    | Zero -> true
    | Succ m -> not (isEven m)

let quattro = Succ (Succ ( Succ( Succ( Zero))))

isEven quattro

let cinque = Succ (quattro)

isEven cinque

type 'T lista =
    | Empty
    | Cons of 'T * ('T lista)

let rec count l =
    match l with
    | Empty -> 0
    | Cons (_,t) -> 1 + count t

let rec count l =
    match l with
    | [] -> 0
    | _::t -> 1 + count t

let rec fib n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | m -> ( fib m-1 ) + ( fib m-2 )

let scambia a b lista =
    let f i =
        if i = 4 then
            2
        else if i = 2 then
            4
        else
            i
    List.permute f lista

let s u = seq {
    let! a = u
    return a
    }

#I @"C:\Users\bartolon\Documents\Visual Studio 2010\Projects\fem\distr\bin\Release"
#I @"C:\Users\pq\Documents\GitHub\fem\distr\bin\Release"
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

let bucket (x:float) (y:seq<float>) =
      y |> Seq.countBy(fun v -> floor(v*x)/x) |> Seq.toList |> Seq.sortBy (fun (x,_)-> x )


let bernoulli p = dist {
    let! u = Dist.uniform
    return u< p
}

let rec binomial x n = dist {
    if n = 0 then
        return 0
    else
        let! b = (bernoulli x) in
        let! r = binomial x (n-1) in
        if b then
            return 1 + r
        else
            return r
}

let point_uniform = dist {
    let! x = Dist.uniform in
        if x > 0.5 then 
            return 0.5
        else
            return 2.0 * x
}


let sampleseq = Dist.getSampleSeq point_uniform (gen())

sampleseq

let coinflip1 = rndvar { return bernoulli 0.5 }
let coinflip2 = rndvar { return bernoulli 0.5 }

let same x y = rndvar {
    let! x = x
    let! y = y
    return dist { return x = y }
}


let d = dist { let! d = getDist (same coinflip1 coinflip2) in return if d then 0.0 else 1.0 }
let sseq = Dist.getSampleSeq d (gen())


Seq.take 100000 sseq |> Seq.average
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
    let! s = Dist.uniform
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

open Distributions
open Dist

let bernoulli p = dist {
        let! uniformSample = uniform
        return p > uniformSample
    }
    
let rec binomial p n = dist {
        if n = 0 then
            return 0
        else       
            let! bernoulliSample = bernoulli p
            let! recursiveSample = binomial p (n-1)
            if bernoulliSample then
                return recursiveSample
            else
                return recursiveSample + 1
    }

let rec exponential l =
    dist {
        let! uSample = uniform //samples uniform, put result in uniformSample
        return - System.Math.Log(1.0-uSample) / l
    }


let pairTable (t:seq<double*'a>) uniform =
        let ru = ref uniform
        t |> Seq.find (fun (p,v) ->
                    ru := (!ru) - p
                    !ru < 0.0
                    )

type Species =
    | A = 0
    | B = 1
    | C = 2
    | D = 3

let reactions = [
    //reagents      products        k
    [0]         ,   [1;2]       ,   0.5
    [0; 1]      ,   [1;3]       ,   0.2
    ]

let probabilityMapper (status:int list) (input:int seq,_,k) =
        input |> Seq.fold (fun cumulative sp -> ( double (status.[int sp]) )* cumulative) k

let initialStatus = [1000000000;0;0;0]

let updateStatus st r =
    let i,o,_ = r
    st |> List.mapi (fun n el-> 
        let el = i|> Seq.fold (fun s r -> if n = int r then (s-1) else s) el 
        let el = o|> Seq.fold (fun s r -> if n = int r then (s+1) else s) el
        el         
        )


let steps = 
    //in order to ensure evaluation of FromDist is performed only once for each step
    Seq.cache (
        seq {
        let mutable lastStatus = random {
            return initialStatus,0.0
        }
        yield lastStatus
        while true do
            let exp = exponential 1.0 |> random.FromDist
            let u = uniform |> random.FromDist
            lastStatus <- random {
                let! status,time = lastStatus
                let probList = reactions |> List.map (probabilityMapper status)
                let probSum = probList |> List.sum
                let! nextTime = exp in let nextTime = nextTime * probSum
                let! selector = u in let _,reaction = pairTable ((probList|> Seq.map (fun x -> x/probSum) |> Seq.zip) reactions ) selector
                return updateStatus status reaction,time + nextTime
                }
            yield lastStatus
        })

let status time =
    let rec euo s = random {
            let! status,t = Seq.head s
            if time< t then
                return None
            else
                let! recurse = euo (s |> Seq.skip 1)
                match recurse with
                | Some x -> return Some x
                | None -> return Some (status,t)                   
        }
    random {
        let! result = euo steps
        match result with
        | Some (s,t) -> return s
        | None -> return initialStatus
        }



// example #: a chain is composed of many links, made using the same production process.
// What's the maximum load we can use the chain for, given its lenght and maximum load for a single ring?
// ring specifications: each link weights 0.1 +- 0.02 kg, and it's breaking load is 100 +-0.1+-0.1 kg
// (first error is same on all rings from same chain, second error is due to random material imperfection)
[<Measure>] type kg

let newRing =
    let systematicLoadError = random.FromDist( Dist.normal 0.0<kg> 0.1<kg>)
    fun () ->
        let ringBreakingLoad = random.FromDist( Dist.normal 100.0<kg> 0.1<kg>)
        let ringWeight = random.FromDist( Dist.normal 0.1<kg> 0.02<kg>)
        random {
        let! systematicError = systematicLoadError
        let! ringBreakingLoad = ringBreakingLoad
        let! ringWeight = ringWeight
        return ringBreakingLoad + systematicError,ringWeight
        }

let rec chain n = random {
        let! headBreakingLoad,headWeight = newRing()
        if n = 1 then
            return headBreakingLoad,headWeight 
        else
            let! tailBreakingLoad,tailWeight = chain (n-1)
            return (min (headBreakingLoad - tailWeight) tailBreakingLoad),tailWeight + headWeight
    }

let hundredRings = getDist (chain 100)
Dist.getSampleSeq hundredRings (gen()) |> Seq.take 1000000 |> Seq.map fst |> Seq.average

type weather = 
    | Rain
    | Sun

let weatherForecast = random {
    let! b = bernoulli 0.25
    return if b then Rain else Sun
    }
    
let willRain = random {
    let! forecast = weatherForecast
    let! forecastWrong = bernoulli 0.1
    match forecast with
    | Sun -> return forecastWrong 
    | Rain -> return not forecastWrong
    }

let maryBringsUmbrella = random {
    let! forecast = weatherForecast
    match forecast with
    | Rain -> return true
    | Sun -> let! bringAnyway = bernoulli 0.2 in return bringAnyway
    }
    
let maryGetsWet = random {
    let! rain = willRain
    let! umbrella = maryBringsUmbrella
    return rain && (not umbrella)
    }
