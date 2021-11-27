type storage = int

let%entry n_primes (num : int) _ =

  let p = 1 in
  let i = 3 in
  let m = (Map [0,2] : (int, int) map) in
  
  let t = (m, p) in

  let i, _ = 
    Loop.left (fun (i, t) ->
        
      let m = t.(0) in
      let p = t.(1) in

      let c = 0 in
      let d = 0 in
      
      let cd = (c, d) in
      
      let _, cd = Loop.left (fun (m, cd) ->
        
        let c = cd.(0) in
        let d = cd.(1) in
          
        let mz = match Map.find c m with
          | None -> failwith ("id is not in the map", c)
          | Some v -> v
        in
          
        let (_, rem) = match i / mz with
          | Some qr -> qr
          | None -> failwith "division by 0 impossible" 
        in
        
        let d =
          if (rem = (0 : nat)) then 1 else d
        in
        
        let c = c + 1 in
        
        let cd = (c, d) in
             
        if (cd.(0) < p) then (Left m, cd)
        else (Right m, cd)
      ) m cd;
      in
      
      let d = cd.(1) in

      let m =
        if (d = 0) then Map.add p i m else m
      in
      
      let p =
        if (d = 0) then p + 1 else p
      in
      
      let t = (m, p) in
        
      if (p < num) then (Left (i + 2), t)
      else (Right i, t)
    ) i t
  in
  
  let i = 
    if (num <= 1) then 2 else i
  in
  
  ( [], i)