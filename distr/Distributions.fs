
//distribuzione: una funzione che prende una tupla di valori casuali, e restituisce un valore.

//sampler: un oggetto che tiene traccia di tutte le variabili aleatorie create, e di quanta entropia e' necessaria per fare un sample di tutte.

//variabile aleatoria: un oggetto con una distribuzione, ed eventualmente correlato a variabili aleatorie precedenti.
module Distributions

//value type for generator (and thus for uniform) samples
type Gen_Sample_T = double

//abstract type for random source generator:
type IGenerator =
    abstract NextValue: IGenerator * Gen_Sample_T

// functional monadic-style wrapper for generator 
let rec genRaw status =
    let randomFunc status =
        let (g:RandomTools.RandomSource),idx = status
        let newStatus = (g,idx+1)
        let value = g.GetSample(idx)
        (newStatus, value)
    {
        new IGenerator with
            member g.NextValue = let status,value = randomFunc status
                                 (genRaw status, value)
    }

// new generator constructor
let gen () =
    genRaw ((new RandomTools.RandomSource () ) ,0)

type IDistribution<'T> =
    abstract NextValue: IGenerator -> IGenerator * 'T
    abstract Value: IGenerator -> 'T
    abstract Next: IGenerator -> IGenerator


type DistributionBuilder () = 
    member d.Bind ((expr:IDistribution<'T>), (f: 'T -> IDistribution<'U>)) =
       {
            new IDistribution<'U> with
                member d.Value g = snd (d.NextValue g)
                member d.Next g = fst (d.NextValue g)
                member d.NextValue g =
                    let g1,v = expr.NextValue g
                    (f v).NextValue g1
        }
    member d.Return (expr:'T) =
        {
            new IDistribution<'T> with
                member d.Value g = snd (d.NextValue g)
                member d.Next g = fst (d.NextValue g)
                member d.NextValue g = (g,expr)
        }
    member d.ReturnFrom (expr:IDistribution<'T>) =
            expr


let dist = new DistributionBuilder()

module Dist =
    let rec getSampleSeq (d:IDistribution<'T>) (g:IGenerator) =
        seq {
            let g1,v = d.NextValue g
            yield v
            yield! getSampleSeq d g1
        }

    let uniform = {
        new IDistribution<double> with
                    member d.NextValue g = g.NextValue
                    member d.Value g = snd (d.NextValue g)
                    member d.Next g = fst (d.NextValue g)
        }

    let normal m s =
        let gaussianBoxMuller (m:float<_>) (sigma:float<_>) =
            dist {
                let! u = uniform
                let! v = uniform
                return m + sigma * sqrt(-2. * log(1.-u)) * cos(2. * System.Math.PI * v)
            }
        gaussianBoxMuller m s