// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.


open Distributions
open RandomVariables
open Dist

let exponential tau = dist {
  let! unif = uniform
  return -tau * log unif
}



type State =
    { time:float
      species: int array }

type Rule =
    { changes:int array
      rate: State -> float }
      
type Ruleset = List<Rule> 

let nextStateSpecies (inputState:State) (rule:Rule) =
    Array.map2 (fun a b -> a + b) inputState.species rule.changes

let selectRule (inputState:State) (ruleset:Ruleset) (u:float) =
    let rates = ruleset |> Seq.map (fun rule -> rule.rate inputState) |> Seq.cache
    let totalRate = rates |> Seq.sum
    let acc = ref 0.0
    totalRate, snd (Seq.zip rates ruleset |> Seq.find (fun (a,b) -> acc := !acc + a ; !acc > u * totalRate))

let nextStep (ruleset:Ruleset) (inputState:State) = random {
    let! u = fromDist uniform 
    let totalRate,rule = selectRule inputState ruleset u
    let! t = exponential (1.0/totalRate) |> fromDist
    return { time=t; species=(nextStateSpecies inputState rule) }
    }

let getEvolution inputState ruleset =
    let rec r s =
        let n s = random { let! s = s in return!  nextStep ruleset s } 
        seq {
            yield s
            yield! r (n s)
        }
    r (random {return inputState } ) 

let section evolution t =
    let rec r ev =
        random {
        let! h = Seq.head ev
        if h.time > t then
            return h
        else
            return! r (Seq.skip 1 ev)
        }
    r evolution



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // restituisci un intero come codice di uscita
