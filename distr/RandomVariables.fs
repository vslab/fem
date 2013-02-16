module RandomVariables
open Distributions

type GenId = int

type RandomVariable =
    | FromDist of obj Distribution * GenId
    | Always of obj
    | Bind of RandomVariable * (obj -> RandomVariable)

type RGen<'T> =
    | R of RandomVariable

type RandomVariableBuilder() =
    let lastId:GenId ref = ref (LanguagePrimitives.GenericZero)

    member x.Bind (a:RGen<'U>,f:'U ->RGen<'T>) =
        RGen<'T>.R (Bind(match a with R b -> b,fun (x:obj) -> match f (x :?> 'U) with R v -> v))

    member x.Bind (a:Distribution<'U>,f:'U -> RGen<'T>) = 
        let id = System.Threading.Interlocked.Increment(lastId)
        if id = LanguagePrimitives.GenericZero then raise (new System.Exception("Maximum number of variable reached"))
        let dobj = dist{let! q = a in return q :> obj}
        let fdist = FromDist(dobj ,id)
        x.Bind(RGen<'U>.R(fdist),f)

    member x.Return (v: 'T) =
        RGen<'T>.R (Always v)

    static member getEnvDist (x:RandomVariable) (env: Map<GenId,obj>) =
        match x with
        | Always v -> dist { return v, env } 
        | FromDist (d,id) -> dist {
            match Map.tryFind id env with
            | Some y -> return y,env
            | None -> let! v = d in return v,env.Add(id,v)
            } 
        | Bind (d,f) -> dist{
            let! va,ea = RandomVariableBuilder.getEnvDist d env
            return! RandomVariableBuilder.getEnvDist (f (va)) ea
        }
 
let random = new RandomVariableBuilder()

let getDist (v:RGen<'T> ) = 
    dist {
        let! vx,ex = RandomVariableBuilder.getEnvDist (match v with R w -> w) Map.empty
        return vx :?>'T
        }
