let rec mapAccumL f c = function
  | [] -> (c,[])
  | (x::xs) -> let (c',x') = f c x in let (cfinal,xs') = mapAccum f c' xs in (cfinal,x'::xs')
let (@) f x = f x
let (%) f g x = f @ g @ x
