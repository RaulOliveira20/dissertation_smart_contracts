type storage = int

let%entry n_primes (num : int) _ =

  let p = 1 in
  let i = 3 in
  let l = [2] in

  let t = (l, p) in

  let i, _ = 
    Loop.left (fun (i, t) ->
        
      let l = t.(0) in
      let p = t.(1) in

      let c = 0 in
      
      let c = List.fold (fun (x, c) ->

        let (_, rem) = match i / x with
          | Some qr -> qr
          | None -> failwith "division by 0 impossible" in
        
        if (rem = (0 : nat)) then 1 else c
             
      ) l c;
      in

      let l =
        if (c = 0) then i :: l else l
      in
      
      let p =
        if (c = 0) then p + 1 else p + 0
      in
      
      let t = (l, p) in
        
      if (p < num) then (Left (i + 2), t)
      else (Right i, t)
    ) i t
  in
  
  let i = 
    if (num <= 1) then 2 else i
  in
  
  ( [], i)