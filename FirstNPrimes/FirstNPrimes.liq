type storage = int  

let%entry n_primes (num : int) _ =

  let p = 1 in
  let i = 3 in

  let _, i =
    Loop.left (fun (p, i) ->
        
          let x = 3 in
        
          let x, i =
            Loop.left (fun (x, i) ->
                
                let (_, rem) = match i / x with
                  | Some qr -> qr
                  | None -> failwith "division by 0 impossible" in
                
                if (rem <> (0 : nat) & x * x <= i) then (Left (x + 2), i)
                else (Right x, i)
            ) x i
          in
          
          let (_, rem) = match i / x with
                  | Some qr -> qr
                  | None -> failwith "division by 0 impossible" in
          
          (*if*)
          
          let p = 
            if (rem <> (0 : nat) || i = 3) then p + 1 else p + 0
          in
          
          if (p < num) then (Left p, (i + 2))
          else (Right p, i)
    ) p i
  in
  
  let i = 
    if (num <= 1) then 2 else i
  in
  
  ( [], i)