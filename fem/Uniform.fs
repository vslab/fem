module Uniform

type samplerEnumerator = System.Collections.Generic.IEnumerator<float>

let seedGenerator = System.Random(1)

let seed = ref 0

let getSeq id = seq { let r = System.Random(id) in while true do yield r.NextDouble() }

let fresh () = seed := seedGenerator.Next(); !seed

let getSource id =
  let s = getSeq id
  let en = s.GetEnumerator()
  en.MoveNext() |> ignore
  en

let nextSample (ss:samplerEnumerator) =
  let s = ss.Current
  ss.MoveNext() |> ignore
  s

let currSample (ss:samplerEnumerator) =
  ss.Current

let moveNext (ss:samplerEnumerator) =
  ss.MoveNext() |> ignore

let samplersDict = new System.Collections.Generic.Dictionary<string,samplerEnumerator>()

let givemeSampler name =
    if (samplersDict.ContainsKey(name)) then
        samplersDict.[name]
    else
        let s = getSource(fresh())
        samplersDict.Add(name, s)
        s

let updateSamplers () =
    samplersDict.Values |> Seq.iter (fun s -> moveNext s )

let makeSeq f =
    seq {
        while (true) do
            updateSamplers()     
            yield f() 
    }

