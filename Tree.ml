#use "Utils.ml"
  
module Tree =
  struct
    type 'a tree = Tree of 'a * (('a tree) list)
    let getData = function
      | (Tree (x,_)) -> x
    let getChildren = function
      | Tree (_,children) -> children
    let rec map f = function
      | Tree (x,cs) -> Tree (f x,List.map (map f) cs)
    let rec fold f a (Tree (x,cs)) = f (List.fold_left (fold f) a cs) x
    let rec mapAccum f c (Tree (x,cs)) = 
      let (newCount,newCs) = mapAccumL (mapAccum f) c cs in
      let (finalC,topX) = f newCount x in
      (finalC,Tree (topX, newCs))
    let enumerate t = snd(mapAccum (fun c x -> (c+1,(c,x))) 0 t)
    let nodeNumber (Tree((n,_),_)) = n
    let rec toList ((Tree(a,ts)) as t) = List.append (List.concat @ List.map toList ts) [t]
    let leftMostDesc = List.hd % toList
    let rec lmld = function
      | ((Tree (a,[])) as t) -> [t]
      | (Tree (a,children)) -> let (lmDesc::rest) = List.concat @ List.map lmld children in List.append (lmDesc::rest) [lmDesc]
    
  end

    
