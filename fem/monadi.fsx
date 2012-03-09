type Generator = System.Random

type Distribution<'T when 'T : comparison > = 
  abstract Sample : 'T

//  abstract Support : Set<'T>
//  abstract Expectation: ('T -> float) -> float

let always x =
  { new Distribution<'T> with 
    member d.Sample = x }

let bind (dist:Distribution<'T>) (k:'T->Distribution<'U>) =
  { new Distribution<'U> with
    member d.Sample =
      (k dist.Sample).Sample }

type DistributionBuilder () =
  member x.Delay f = bind (always()) f
  member x.Bind (d,f) = bind d f
  member x.Return v = always v
  member x.ReturnFrom vs = vs

let dist = new DistributionBuilder()

module Dist = 
  let toSeq (x:Distribution<'T>) =
    seq{ while true do yield x.Sample }

let uniform (g:Generator) = //tra zero e 1
  { new Distribution<float> with
    member d.Sample =  g.NextDouble() }

let bernoulli p g =
  dist {
    let! u = uniform g
    return u>p
  }

let gaussianBoxMuller m sigma (s:Generator) =
  dist {
    let! u = uniform s
    let! v = uniform s
    return m + sigma * sqrt(-2. * log(u)) * cos(2. * System.Math.PI * v)
  }

dist {
  let! u = gaussianBoxMuller 1. 0. (new Generator())
  return 2. * u
}
//let binomial p n0 (s:Generator) =
//  dist {
//    let bernoullip = bernoulli p
//    let rec binomialp n =
//      if n = 0 then 0.
//      else 
//        let! b = bernoullip s
//        let x = binomialp (n - 1)
//        if b then 1. + x else x
//    return binomialp n0
//  }

seq {
  for v in 0 .. 100 do
    let g = gaussianBoxMuller 0. 1. (new Generator(v)) |> Dist.toSeq
    let count = 100000
    let m, v = g |> Seq.take count |> Seq.fold (fun (m, v) x -> (m + x, v + x * x)) (0., 0.)|> fun (m, v) -> let c = float(count) in let avg = m / c in (avg, sqrt(v / c - avg * avg))
    yield (m, v)
} |> Seq.fold (fun (m, v) (xm, xv) -> (m + xm, v + xv)) (0., 0.) |> fun (m, v) -> (m / 100., v / 100.)



bernoulli 0.5 |> Dist.toSeq |> Seq.take 10
