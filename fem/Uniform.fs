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
