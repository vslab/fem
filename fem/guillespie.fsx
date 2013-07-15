#I @"C:\Users\bartolon\Documents\Visual Studio 2010\Projects\fem\distr\bin\Release"
#I @"C:\Users\pq\Documents\GitHub\fem\distr\bin\Release"
#I @"C:\Users\Davide\Desktop\Projects\fem\fem\bin\Debug"
#I @"C:\progetti\fem\distr\bin\Debug"

#r @"RandomTools.dll"
#r @"distr.dll"


open Distributions
open Dist
open RandomVariables

let bernoulli p = dist {
        let! uniformSample = uniform
        return p > uniformSample
    }
    
let rec binomial p n = dist {
        if n = 0 then
            return 0
        else       
            let! bernoulliSample = bernoulli p
            let! recursiveSample = binomial p (n-1)
            if bernoulliSample then
                return recursiveSample
            else
                return recursiveSample + 1
    }

let rec exponential l =
    dist {
        let! uSample = uniform //samples uniform, put result in uniformSample
        return - System.Math.Log(1.0-uSample) / l
    }


let pairTable (t:seq<double*'a>) uniform =
        let ru = ref uniform
        t |> Seq.find (fun (p,v) ->
                    ru := (!ru) - p
                    !ru < 0.0
                    )

type Species =
    | A = 0
    | B = 1
    | C = 2
    | D = 3

let reactions = [
    //reagents      products        k
    [0]         ,   [1;2]       ,   0.5
    [0; 1]      ,   [1;3]       ,   0.2
    ]

let probabilityMapper (status:int list) (input:int seq,_,k) =
        let sum = float (status |> List.sum)
        input |> Seq.fold (fun cumulative sp -> ( double (status.[int sp]) )* cumulative / sum) k

let initialStatus = [10000;0;0;0]

let updateStatus st r =
    let i,o,_ = r
    st |> List.mapi (fun n el-> 
        let el = i|> Seq.fold (fun s r -> if n = int r then (s-1) else s) el 
        let el = o|> Seq.fold (fun s r -> if n = int r then (s+1) else s) el
        el         
        )


let steps = 
    //in order to ensure evaluation of FromDist is performed only once for each step
    Seq.cache (
        seq {
        let lastStatus = ref (random {
            return initialStatus,0.0
        })
        yield !lastStatus
        while true do
            let exp = exponential 1.0 |> random.FromDist
            let u = uniform |> random.FromDist
            lastStatus := random {
                let! status,time = !lastStatus
                let probList = reactions |> List.map (probabilityMapper status)
                let probSum = probList |> List.sum
                let! nextTime = exp in let nextTime = nextTime * probSum
                let! selector = u in let _,reaction = pairTable ((probList|> Seq.map (fun x -> x/probSum) |> Seq.zip) reactions ) selector
                return updateStatus status reaction,time + nextTime
                }
            yield !lastStatus
        })

let status time =
    let rec euo s = random {
            let! status,t = Seq.head s
            if time< t then
                return None
            else
                let! recurse = euo (s |> Seq.skip 1)
                match recurse with
                | Some x -> return Some x
                | None -> return Some (status,t)                   
        }
    random {
        let! result = euo steps
        match result with
        | Some (s,t) -> return s
        | None -> return initialStatus
        }

let speciesDistAt2 = status 400. |> getDist
Dist.getSampleSeq (speciesDistAt2) (gen())
Dist.getSampleSeq (steps |> Seq.skip 1 |> Seq.head |> getDist) (gen())