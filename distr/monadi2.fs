
//distribuzione: una funzione che prende una tupla di valori casuali, e restituisce un valore.

//sampler: un oggetto che tiene traccia di tutte le variabili aleatorie create, e di quanta entropia e' necessaria per fare un sample di tutte.

//variabile aleatoria: un oggetto con una distribuzione, ed eventualmente correlato a variabili aleatorie precedenti.

type T = double

type Generator =
    abstract Value: T
    abstract Next: Generator

let gen seed =
    let randomSeq =  seq{let r = new System.Random(seed) in while true do yield r.NextDouble() }
    let rec genFromSeq s = {
           new Generator with
           member g.Value = Seq.head s
           member g.Next = s |> Seq.skip 1 |> genFromSeq
        }
    genFromSeq randomSeq

type Distribution<'T> =
    abstract Value: Generator -> 'T
    abstract Next: Generator -> Generator

type DistributionBuilder () = 
    member d.Bind ((expr:Distribution<'T>), (f: 'T -> Distribution<'U>)) =
        let v g = expr.Value g
        let n g = expr.Next g 
        {
            new Distribution<'U> with
                member d.Value g = (v g |> f).Value (n g)
                member d.Next g = (v g |> f).Next (n g)
        }
    member d.Return (expr:'T) =
        {
            new Distribution<'T> with
                member d.Value g = expr
                member d.Next g = g
        }


let dist = new DistributionBuilder()

module Dist =
    let rec toRandom (d:Distribution<'T>) (g:Generator) = seq {
            yield d.Value g
            yield! toRandom d (d.Next g)
    }

let uniform = {
    new Distribution<double> with
                member d.Value g = g.Value
                member d.Next g = g.Next
    }

let sei = dist { return 6 }

let gaussianBoxMuller (m:double) (sigma:double) =
  dist {
    let! u = uniform
    let! v = uniform
    return m + sigma * sqrt(-2. * log(u)) * cos(2. * System.Math.PI * v)
  }



let const1 x = dist { return x }

let const2 x = dist { let! u = uniform in return x }



(*

let sample d: Distribution<'T> -> ('T , Distribution<'T>)=
    return d

let gaussianBoxMuller m sigma u = dist {
    let! first = sample u
    let! second = sample u
    return m + sigma * sqrt(-2. * log(first)) * cos(2. * System.Math.PI * second)
}
let myGauss = Distr.toAleatoria (GaussianBoxMuller 3.14 0.618)
let myGauss2 = Distr.toAleatoria (GaussianBoxMuller 3.14 0.618)
let myGauss3 = myGauss2
let a = myGauss + myGauss2
let b = a - myGauss3
let SampleSeq = b.Samples

let plus a b =  new Aleatoria<'U> with
    member this.Samples = zip a.Samples b.Samples |> Seq.map plus
*)


