
//distribuzione: una funzione che prende una tupla di valori casuali, e restituisce un valore.

//sampler: un oggetto che tiene traccia di tutte le variabili aleatorie create, e di quanta entropia e' necessaria per fare un sample di tutte.

//variabile aleatoria: un oggetto con una distribuzione, ed eventualmente correlato a variabili aleatorie precedenti.
module Distributions

type T = double

type Generator =
    abstract NextValue: Generator * T

let randomFuncOrig status =
    let newStatus = (new System.Random(status)).Next()
    let value = (new System.Random(status)).NextDouble()
    (newStatus,value)

let randomFunc status =
    let (g:RandomTools.RandomSource),idx = status
    let newStatus = (g,idx+1)
    let value = g.GetSample(idx)
    (newStatus, value)

let rec genRaw status =
    {
        new Generator with
            member g.NextValue = let status,value = randomFunc status
                                 (genRaw status, value)
    }


let gen () =
    genRaw ((new RandomTools.RandomSource () ) ,0)

type Distribution<'T> =
    abstract NextValue: Generator -> Generator * 'T
    abstract Value: Generator -> 'T
    abstract Next: Generator -> Generator


type DistributionBuilder () = 
    member d.Bind ((expr:Distribution<'T>), (f: 'T -> Distribution<'U>)) =
       {
            new Distribution<'U> with
                member d.Value g = snd (d.NextValue g)
                member d.Next g = fst (d.NextValue g)
                member d.NextValue g =
                    let g1,v = expr.NextValue g
                    (f v).NextValue g1
        }
    member d.Return (expr:'T) =
        {
            new Distribution<'T> with
                member d.Value g = snd (d.NextValue g)
                member d.Next g = fst (d.NextValue g)
                member d.NextValue g = (g,expr)
        }
    member d.ReturnFrom (expr:Distribution<'T>) =
            expr


let dist = new DistributionBuilder()

module Dist =
    let rec toRandomRaw (d:Distribution<'T>) (g:Generator) = seq {
            let g1,v = d.NextValue g
            yield v
            yield! toRandomRaw d g1
    }
    let toRandom d = RandomVariables.RandomVariable(toRandomRaw d (gen ()))

let uniform = {
    //samples: uniform values in [0,1)
    new Distribution<double> with
                member d.NextValue g = g.NextValue
                member d.Value g = snd (d.NextValue g)
                member d.Next g = fst (d.NextValue g)
    }

let gaussianBoxMuller (m:double) (sigma:double) =
  dist {
    let! u = uniform
    let! v = uniform
    return m + sigma * sqrt(-2. * log(1.-u)) * cos(2. * System.Math.PI * v)
  }

let constDist x = dist { return x }

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


