module RandomVariables
open Distributions

type GenId = int

type RandomVariable =
    | FromDist of obj IDistribution * GenId
    | Always of obj
    | Bind of RandomVariable * (obj -> RandomVariable)

type RGen<'T> =
    | R of RandomVariable

type RandomVariableBuilder() =
    let lastId:GenId ref = ref (LanguagePrimitives.GenericZero)
        
    member x.Bind (a:RGen<'U>,f:'U ->RGen<'T>) =
        RGen<'T>.R (Bind(match a with R b -> b,fun (x:obj) -> match f (x :?> 'U) with R v -> v))

    member x.FromDist (d:IDistribution<'T>) =
        let id = System.Threading.Interlocked.Increment(lastId)
        let dobj = dist{let! q = d in return q :> obj}
        RGen<'T>.R(FromDist(dobj,id))

    member x.BindDist (a:IDistribution<'U>,f:'U -> RGen<'T>) = 
        x.Bind(x.FromDist(a),f)

    member x.Return (v: 'T) =
        RGen<'T>.R (Always v)

    member x.ReturnFrom (v:RGen<'T>) =
        v

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
 
let _rndvar = new RandomVariableBuilder()


let fromDist (d:IDistribution<'T>) =
    _rndvar.FromDist d

let _getDist (v:RGen<'T> ) = 
    dist {
        let! vx,ex = RandomVariableBuilder.getEnvDist (match v with R w -> w) Map.empty
        return vx :?>'T
        }


type NewRandomVariable =
    | Independent of obj IDistribution * GenId
    | Bind of NewRandomVariable * (obj -> NewRandomVariable)

type NewRGen<'T> =
    | R of NewRandomVariable

type NewRandomBuiler() =
    let lastId:GenId ref = ref (LanguagePrimitives.GenericZero)
        
    member x.Bind (a:NewRGen<'U>,f:'U ->NewRGen<'T>) =
        NewRGen<'T>.R (Bind(match a with R (b) -> b,fun (x:obj) -> match f (x :?> 'U) with R (v) -> v))

    member x.Return (d:IDistribution<'T>) =
        let id = System.Threading.Interlocked.Increment(lastId)
        let dobj = dist{let! q = d in return q :> obj}
        NewRGen<'T>.R (Independent(dobj,id))

let getDist (r:NewRGen<'U>) =
    let rec memoizedSampling (context:Map<GenId,obj>) (var:NewRandomVariable) =
        match var with
        | Independent (E,id) -> 
            if context.ContainsKey(id) then
                dist { return context,context.Item(id) }
            else
                dist { let! s = E in return context.Add(id,s),s }
        | Bind (r,f) ->
            dist {
                let! context,v = memoizedSampling context r
                let! context,v = memoizedSampling context (f v)
                return context,v
                }
    match r with
    | R r ->
    dist {
        let! context,v = memoizedSampling Map.empty r
        return v:?> 'U
        }
      
let rndvar = new NewRandomBuiler()
