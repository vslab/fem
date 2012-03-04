// Learn more about F# at http://fsharp.net

module Module1

type samplerEnumerator = Uniform.samplerEnumerator

let gaussianBoxMuller m sigma (s:samplerEnumerator) =
  let u = Uniform.nextSample s
  let v = Uniform.nextSample s
  m + sigma * sqrt(-2. * log(u)) * cos(2. * System.Math.PI * v)

let bernoulli p (s:samplerEnumerator) =
  let u = Uniform.nextSample s
  u <= p

let binomial p n0 (s:samplerEnumerator) =
    let bernoullip = bernoulli p
    let rec binomialp n =
        if n = 0 then 0.
        else
            let x = binomialp (n - 1)
            let b = bernoullip s
            if b then 1. + x else x
    binomialp n0



let delta x (_:samplerEnumerator) =
  x

        
let sample func =
    seq {
      let ss = Uniform.fresh() |> Uniform.getSource
      while true do yield (ss |> func)
    }


let bucket (x:float) y =
      y |> Seq.countBy(fun v -> floor(v*x)/x) |> Seq.toList |> Seq.sortBy (fun (x,_)-> x )

