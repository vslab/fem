
//#I @""
//#r @"RandomTools
#if INTERACTIVE
#I @"C:\Users\pq\Documents\GitHub\fem\distr\bin\Release"
#I @"C:\Users\pq\Documents\GitHub\fem\distr\bin\Debug"
#r @"distr.dll"
#endif
open Distributions
open RandomVariables
 // example #1: a farmaceutical company has developed a new drug.
 // We want to validate the production process to ensure the final product is
 // within specification.

 //Specification for final product:
 // Ichinox     185+-5 ppm
 // Nirox       125+-5 ppm
 // Sabulax     340+-10 ppm
 // Mifoshitan  615+-10 ppm
 // Golanapen   34 +- 2 ppm



 // example #: a chain is composed of many links, made using the same production process.
 // What's the maximum load we can use the chain for, given its lenght and maximum load for a single ring?
 // ring specifications: each link weights 0.1 +- 0.02 kg, and it's breaking load is 100 +-0.1+-0.1 kg
 // (first error is same on all rings from same chain, second error is due to random material imperfection)
[<Measure>] type kg

let oneRingBreakingLoad =
    let systematicError = Dist.toRandom( gaussianBoxMuller 0.0<kg> 0.1<kg>)
    let load () =
        let l = Dist.toRandom( gaussianBoxMuller 100.0<kg> 0.1<kg>)
        l .+ systematicError
    load

let rec BreakingLoad n = 
    let r = oneRingBreakingLoad ()
    if n = 1 then
        r
    else
        RandomVariable.zipApply (RandomVariable.zipApply (Random.always min) r) ((BreakingLoad (n - 1)) .- (Dist.toRandom (gaussianBoxMuller 0.1<kg> 0.02<kg>)))

let sqrt (value:float<'u^2>) =
    (box (System.Math.Sqrt ((float) value)) ) :?> float<'u>

let a = oneRingBreakingLoad ()
let b = BreakingLoad 200
let standardDeviation (v:RandomVariable<float<_>>) = sqrt ( ( Random.meanOver 5000 (v .* v) ) - let m = (Random.meanOver 5000 v) in m * m)

//one ring breaking load:
System.String.Format("Breaking load for 1 ring: {0} +- {1}", (Random.meanOver 5000 a ) ,standardDeviation a)
//200 rings breaking load
System.String.Format("Breaking load for 200 rings: {0} +- {1}", (Random.meanOver 5000 b ) ,standardDeviation b)


//example: we have an electric circuit with a fragile component, which will break if dissipated power is over 30 Watt
// We want to evaluate the chance of it breaking.

[<Measure>] type V
[<Measure>] type A
[<Measure>] type Ohm = V/A
[<Measure>] type W = V * A
let step a b = if a < b then 1. else 0.

//input power source 
let powerSource = Dist.toRandom (gaussianBoxMuller 12.<V> 1.0<V> )

//load and protection resistances are on series link. Shunt resistance is on parallel link with both others
let protectionResistance = Dist.toRandom (gaussianBoxMuller 1.0<Ohm> 0.05<Ohm> )
let shuntResistance = Dist.toRandom (gaussianBoxMuller 1.0<Ohm> 0.05<Ohm> )
let loadResistance = Dist.toRandom (gaussianBoxMuller 0.8<Ohm> 0.04<Ohm> )

let current = powerSource ./ (protectionResistance .+ loadResistance)

let power = current .* current  .* loadResistance

let breakingPower = 40.0<W>
let willBreak = RandomVariable.zipApply  (Random.always (step breakingPower) ) power
let breakChance = Random.meanOver 1000 willBreak

//we aren't satisfied, so we decide to put a fuse near power source. Fuse will break if current running through it is over 

//fuse real breaking power is up to 5% off it's nominal breaking current
let fuseNominalBreakingCurrent = 7.3<A>
let fuseBreakingCurrent = Random.always(fuseNominalBreakingCurrent) .* Dist.toRandom (gaussianBoxMuller 1.0 0.05)

let fuseWillBreak = RandomVariable.zipApply (RandomVariable.zipApply (Random.always step) fuseBreakingCurrent) current
let fuseBreakChance = Random.meanOver 1000 fuseWillBreak


//if power is enough to destroy load, but current is not enough to break fuse we would lose load anyway
let willBreakWithFuseFun lb fb = if (lb > 0.5) && (fb < 0.5) then 1.0 else 0.0
let willBreakWithFuse = RandomVariable.zipApply (RandomVariable.zipApply (Random.always willBreakWithFuseFun) willBreak) fuseWillBreak
//the device will now break with chance
let breakChanceWithFuse = Random.meanOver 1000 willBreakWithFuse