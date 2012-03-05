// Learn more about F# at http://fsharp.net

module Module1

type samplerEnumerator = Uniform.samplerEnumerator

let gaussianBoxMuller m sigma (s1:samplerEnumerator) (s2:samplerEnumerator) =
  let u = Uniform.currSample s1
  let v = Uniform.currSample s2
  m + sigma * sqrt(-2. * log(u)) * cos(2. * System.Math.PI * v)

let bernoulli p (s:samplerEnumerator) =
  let u = Uniform.currSample s
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



let bucket (x:float) y =
      y |> Seq.countBy(fun v -> floor(v*x)/x) |> Seq.toList |> Seq.sortBy (fun (x,_)-> x )


