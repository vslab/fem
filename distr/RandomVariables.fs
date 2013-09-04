module RandomVariables
open Distributions

type GenId = int

type RandomVariable =
    | FromDist of obj IDistribution * GenId
    | Always of obj
    | Bind of RandomVariable * (obj -> RandomVariable)

type Bdd = 
    | True
    | False
    | Fork of GenId * Bdd * Bdd

type Event =
    | True
    | False
    | Var of bool IDistribution * GenId
    | Bind of Event * Event * Event

type RGen<'T> =
    | R of RandomVariable


fun eval b = 
type EventBuilder() =
    let lastId: GenId ref = ref (LanguagePrimitives.GenericZero)

    member x.Bind(a:Event, f:bool -> Event) =
        match a with
        | True -> f true
        | False -> f false
        | Bind (e,f,g) -> s
        | Var (d,id)-> Bdd.Fork
        
    member x.Return (v:bool) =
        if v then Event.True else Event.False

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
 
let rndvar = new RandomVariableBuilder()

let event = new EventBuilder()

let fromDist (d:IDistribution<'T>) =
    random.FromDist d

let getDist (v:RGen<'T> ) = 
    dist {
        let! vx,ex = RandomVariableBuilder.getEnvDist (match v with R w -> w) Map.empty
        return vx :?>'T
        }
