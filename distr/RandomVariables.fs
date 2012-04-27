module RandomVariables

type RandomVariable<'T> (samples:seq<'T>) = 
    member this.Samples = samples
    static member lift f = fun (r:RandomVariable<'T>) -> new RandomVariable<'U>(r.Samples |> Seq.map f)
    static member lift2 f = fun (x:RandomVariable<'T>,y:RandomVariable<'U>) -> new RandomVariable<'S>(Seq.zip x.Samples y.Samples |> Seq.map f)

let inline ( .+ )  a b = RandomVariable.lift2 (fun (x,y) -> x + y) (a,b)
let inline ( .- )  a b = RandomVariable.lift2 (fun (x,y) -> x - y) (a,b)
let inline ( .* )  a b = RandomVariable.lift2 (fun (x,y) -> x * y) (a,b)
let inline ( ./ )  a b = RandomVariable.lift2 (fun (x,y) -> x / y) (a,b)
let inline ( .% )  a b = RandomVariable.lift2 (fun (x,y) -> x % y) (a,b)
let inline ( .** ) a b = RandomVariable.lift2 (fun (x,y) -> x ** y) (a,b)

let inline ( .= )  a b = RandomVariable.lift2 (fun (x,y) -> x = y) (a,b)
let inline ( .< )  a b = RandomVariable.lift2 (fun (x,y) -> x < y) (a,b)
let inline ( .> )  a b = RandomVariable.lift2 (fun (x,y) -> x > y) (a,b)
let inline ( .<= ) a b = RandomVariable.lift2 (fun (x,y) -> x <= y) (a,b)
let inline ( .>= ) a b = RandomVariable.lift2 (fun (x,y) -> x >= y) (a,b)
let inline ( .!= ) a b = RandomVariable.lift2 (fun (x,y) -> x != y) (a,b)

//let inline ( ~.+ ) a = RandomVariable.lift (fun x -> +x) a
//let inline ( ~.- ) a = RandomVariable.lift (fun x -> -x) a

module Random =
    let always x = RandomVariable(seq { while true do yield x })