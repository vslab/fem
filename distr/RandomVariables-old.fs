module RandomVariables



type RandomVariable<'T> (samples:seq<'T>) = 
    static member always x = new RandomVariable<'T>(seq { while true do yield x })

    member this.Samples = samples
    static member zipApply (funct:RandomVariable<'T->'U>) (param:RandomVariable<'T>) :RandomVariable<'U> = 
        new RandomVariable<'U>( Seq.map2 (fun x y -> (x y)) funct.Samples param.Samples )
    static member lift (f:'T->'U) g = RandomVariable.zipApply (RandomVariable.always f) g
    //fun (r:RandomVariable<'T>) -> new RandomVariable<'U>(r.Samples |> Seq.map f)
    static member lift2 (f:'T->'U->'V) g h = RandomVariable.zipApply (RandomVariable.lift f g) h
    //fun (x:RandomVariable<'T>,y:RandomVariable<'U>) -> new RandomVariable<'S>(Seq.zip x.Samples y.Samples |> Seq.map f)

let inline ( .+ )  a b = RandomVariable.lift2 (fun x y -> x + y) a b
let inline ( .- )  a b = RandomVariable.lift2 (fun x y -> x - y) a b
let inline ( .* )  a b = RandomVariable.lift2 (fun x y -> x * y) a b
let inline ( ./ )  a b = RandomVariable.lift2 (fun x y -> x / y) a b
let inline ( .% )  a b = RandomVariable.lift2 (fun x y -> x % y) a b
let inline ( .** ) a b = RandomVariable.lift2 (fun x y -> x ** y) a b

let inline ( .= )  a b = RandomVariable.lift2 (fun x y -> x = y) a b
let inline ( .< )  a b = RandomVariable.lift2 (fun x y -> x < y) a b
let inline ( .> )  a b = RandomVariable.lift2 (fun x y -> x > y) a b
let inline ( .<= ) a b = RandomVariable.lift2 (fun x y -> x <= y) a b
let inline ( .>= ) a b = RandomVariable.lift2 (fun x y -> x >= y) a b
let inline ( .!= ) a b = RandomVariable.lift2 (fun x y -> x != y) a b

//let inline ( ~.+ ) a = RandomVariable.lift (fun x -> +x) a
//let inline ( ~.- ) a = RandomVariable.lift (fun x -> -x) a

module Random =
    let always x = RandomVariable.always x//(seq { while true do yield x })
    let inline meanOver< ^T when ^T:(static member ( + ) : ^T * ^T -> ^T) and
                                 ^T:(static member DivideByInt : ^T * int -> ^T) and
                                 ^T:(static member Zero: ^T ) > sampleCount ( meanableVariable:RandomVariable< ^T >) = 
            meanableVariable.Samples |> Seq.take sampleCount |> Seq.average

// Cosa ho imparato nelle 3 ore che ci ho messo a scrivere questa funzione:
    //1. i type constraint si possono mettere solo se si fa una dichiarazione formale dei tipi generici, ovvero subito dopo il nome della funzione/tipo
    //2. sintassi : -> e' solo per il compilatore, ci riporta gli errori. Si puo' chiedere la presenza di una property direttamente col nome di una property
    //3. constraint su presenza di membri statici si possono fare solo a tempo di compilazione, quindi si deve usare ^T e non 'T
    //4. la risoluzione dei tipi a tempo di compilazione ( ^ ) si puo' fare solo su funzioni inline, e viceversa
