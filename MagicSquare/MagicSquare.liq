type storage = (int, int) map

let%entry magic_square (num : int) _ =
  
  let a = (Map [] : (int, int) map) in
  let b = (Map [] : (int, int) map) in
  let used = (Map [] : (int, int) map) in
  let m = (Map [] : (int, int) map) in 
  let i = 0 in
  let j = 0 in(*
  let s = 0 in
  let k = 0 in
  let t = 0 in
  let p = 0 in*)
  let back = false in
  let b_o = 0 in
  
  let (t, _) = match (num - 1) / 2 with
    | Some qr -> qr
    | None -> failwith "division by 0 impossible" in
  
  let k = num - 1 in
  
  let ab = (a, b) in
  let ijk = (i, j, k) in
  
  let ab, _ = 
    Loop.left (fun (ab, ijk) ->
      let (_, r) = match num / 2 with
        | Some qr -> qr
        | None -> failwith "division by 0 impossible" in
    
      let t = 
        if (r = (0 : nat)) then ijk.(0) else t
      in
      
      let s = ijk.(0) * num + ijk.(1) in
      let p = ijk.(0) * num + ijk.(2) in
    
      let a = ab.(0) in
      let b = ab.(1) in
      let a = Map.add s t a in
      let b = Map.add p t b in
      let ab = (a, b) in

      let i = ijk.(0) in
      let j = ijk.(1) in
      let k = ijk.(2) in
      let i = i + 1 in
      let j = j + 1 in
      let k = k - 1 in
      let ijk = (i, j, k) in
    
      if (ijk.(0) < num) then (Left ab, ijk)
      else (Right ab, ijk)
    ) ab ijk;
  in
  
  let i = num - 1 in
  let j = 0 in
  let p = 0 in
  
  let (t, _) = match (num - 1) / 2 with
    | Some qr -> qr
    | None -> failwith "division by 0 impossible" in
  
  let (_, r) = match num / 2 with
    | Some qr -> qr
    | None -> failwith "division by 0 impossible" in
  
  let used =
    if r = (1 : nat) then Map.add t 1 used else used
  in
  
  let m =
    if r = (1 : nat) then Map.add (t * num + t) 1 m else m
  in
  
  let ijp = (i, j, p) in
  let a = ab.(0) in
  let b = ab.(1) in
  let abum = (a, b, used, m) in
  
  let abum, _ = 
    Loop.left (fun (abum, ijp) ->
      let ijp = 
        if (ijp.(0) = ijp.(1) & num <> 1) then
          let i = ijp.(0) - 1 in
          let j = ijp.(1) + 1 in
          let p = ijp.(2) + 1 in
          (i, j, p)
        else
          ijp
      in
      
      let s = ijp.(0) * num + ijp.(1) in
      
      let k = 0 in
      let used = abum.(2) in
      
      let k = 
        if (r = (1 : nat)) then
          let k, _ = Loop.left (fun (k, used) ->
            let mz = match Map.find k used with
              | None -> 0
              | Some v -> v
            in
            
            if (mz = 1) then (Left (k + 1), used)
            else (Right k, used)
          ) k used;
          in
          k
        else
          let k, _ = Loop.left (fun (k, used) ->
            let mz = match Map.find k used with
              | None -> 0
              | Some v -> v
            in
            
            if (mz = 1 || k = ijp.(0) || k = ijp.(1)) then (Left (k + 1), used)
            else (Right k, used)
          ) k used;
          in
          k
      in
      
      let a = abum.(0) in
      let used = abum.(2) in
      
      let a = Map.add s k a in
      let used = Map.add k 1 used in
      
      let b = abum.(1) in
      let m = abum.(3) in
      
      let x = match Map.find s b with
        | None -> 0
        | Some v -> v
      in
      
      let s = k * num + x in
      let m = Map.add s 1 m in
      
      let s = ijp.(2) * num + ijp.(1) in
      let b = Map.add s k b in
      
      let x = match Map.find s a with
        | None -> 0
        | Some v -> v
      in
      
      let s = x * num + k in
      let m = Map.add s 1 m in
      
      let (i, j, p) = ijp in
      let i = i - 1 in
      let j = j + 1 in
      let p = p + 1 in
      
      let ijp = (i, j, p) in
      let abum = (a, b, used, m) in
      
      if (ijp.(1) < num) then (Left abum, ijp)
      else (Right abum, ijp)
    ) abum ijp;
  in

  let i = 0 in
  let j = 0 in
  let abmij = (abum.(0), abum.(1), abum.(3), i, j) in
  let backbo = (back, b_o) in
  
  let abmij, _ =
    Loop.left (fun (abmij, backbo) ->
      let t = abmij.(3) + abmij.(4) in
      
      let abmijbackbo = 
        if (abmij.(3) = abmij.(4) || t = (num - 1)) then
          let abmij = 
            if (backbo.(0) = false) then
              let abmij = 
                if (abmij.(4) < (num - 1)) then
                  let j = abmij.(4) + 1 in
                  (abmij.(0), abmij.(1), abmij.(2), abmij.(3), j)
                else
                  let i = abmij.(3) + 1 in
                  let j = 0 in
                  (abmij.(0), abmij.(1), abmij.(2), i, j)
              in
              abmij
            else
              let abmij = 
                if (abmij.(4) > 0) then
                  let j = abmij.(4) - 1 in
                  (abmij.(0), abmij.(1), abmij.(2), abmij.(3), j)
                else
                  let i = abmij.(3) - 1 in
                  let j = num - 1 in
                  (abmij.(0), abmij.(1), abmij.(2), i, j)
              in
              abmij
          in
          (abmij.(0), abmij.(1), abmij.(2), abmij.(3), abmij.(4), backbo.(0), backbo.(1))
        else
          let used = Map [] in
          let use = Map [] in
          let k = 0 in
          let a = abmij.(0) in
          let b = abmij.(1) in
          let m = abmij.(2) in
          let i = abmij.(3) in
          let j = abmij.(4) in
          
          let uu = (used, use) in
          
          let uu, _ = 
            Loop.left (fun (uu, k) ->
            
            let used = uu.(0) in
            let use = uu.(1) in
            
            let uu = 
              if (k <> j) then
                let s = i * num + k in
                
                let p = match Map.find s a with
                  | None -> num
                  | Some v -> v
                in
                
                let used = 
                  if (p <> num) then
                    Map.add p 1 used
                  else
                    used
                in
                
                let p = match Map.find s b with
                  | None -> num
                  | Some v -> v
                in
                
                let use = 
                  if (p <> num) then
                    Map.add p 1 use
                  else
                    use
                in
                
                (used, use)
              else
                (used, use)
            in
            
            let used = uu.(0) in
            let use = uu.(1) in
            
            let uu = 
              if (k <> i) then
                let s = k * num + j in
                
                let p = match Map.find s a with
                  | None -> num
                  | Some v -> v
                in
                
                let used = 
                  if (p <> num) then
                    Map.add p 1 used
                  else
                    used
                in
                
                let p = match Map.find s b with
                  | None -> num
                  | Some v -> v
                in
                
                let use = 
                  if (p <> num) then
                    Map.add p 1 use
                  else
                    use
                in
                
                (used, use)
              else
                (used, use)
            in
            
            let k = k + 1 in
            
            if (k < num) then (Left uu, k)
            else (Right uu, k)
            ) uu k;
          in
          
          let used = uu.(0) in
          let use = uu.(1) in
          
          let used =
            if (i = 1) & (j = 0) then
              let k = 0 in
              
              let (q, r) = match num / 2 with
              | Some qr -> qr
              | None -> failwith "division by 0 impossible" in
              
              let t = 
                if (r = (0 : nat)) then q else (num - 1)
              in
              
              let used, _ = 
                Loop.left (fun (used, k) ->

                let used = Map.add k 1 used in
                let k = k + 1 in
                              
                if (k < t) then (Left used, k)
                else (Right used, k)
                ) used k;
              in
              used
            else
              used
          in
          
          let use =
            if (i = 1) & (j = 2) then
              let k = 0 in
              
              let (q, _) = match num / 2 with
              | Some qr -> qr
              | None -> failwith "division by 0 impossible" in
              let t = q - 1 in
              
              let use = if (t > 0) then
                let use, _ = 
                  Loop.left (fun (use, k) ->

                  let use = Map.add k 1 use in
                  let k = k + 1 in
                
                  if (k < t) then (Left use, k)
                  else (Right use, k)
                  ) use k;
                in
                use
              else
                use
              in
              
              use
            else
              use
          in
          
          let use =
            if (i = 1) & (j = 3) then
              let k = 0 in
              let t = 3 in
              
              let use, _ = 
                Loop.left (fun (use, k) ->

                let use = Map.add k 1 use in
                let k = k + 1 in
                              
                if (k < t) then (Left use, k)
                else (Right use, k)
                ) use k;
              in
              use
            else
              use
          in
          
          let abmkt = (a, b, m, 0, 0) in
          
          let abmkt =
            if (backbo.(0) = true) then
              let s = i * num + j in
              let k = match Map.find s abmkt.(0) with
                | None -> num
                | Some v -> v
              in
              let t = match Map.find s abmkt.(1) with
                | None -> num
                | Some v -> v
              in
              let a = Map.add s num abmkt.(0) in
              let b = Map.add s num abmkt.(1) in
              
              let s = k * num + t in
              let m = Map.add s 0 abmkt.(2) in
              
              let tk = (t, k) in
              let tk =
                if (backbo.(1) = 2) then
                  (tk.(0) + 1, tk.(1) + 1)
                else
                  let tk = 
                    if (backbo.(1) = 1) then
                      (0, tk.(1) + 1)
                    else
                      (tk.(0) + 1, tk.(1))
                  in
                  tk
              in
              (a, b, m, tk.(1), tk.(0))
            else
              (abmkt.(0), abmkt.(1), abmkt.(2), 0, 0)
          in
          
          let b_o = backbo.(1) in
          let b_o =
            if (backbo.(0) = true) then
              b_o
            else
              0
          in
          let backbo = (backbo.(0), b_o) in
          
          let k = abmkt.(3) in
          let t = abmkt.(4) in
          (*
          let ijkt = 
            if (i = 4 & j = 3) then
              (num + 1, num + 1, num + 1, num + 1)
            else
              (i, j, k, t)
          in
          let i = ijkt.(0) in
          let j = ijkt.(1) in
          let k = ijkt.(2) in
          let t = ijkt.(3) in
          *)
          let k, _ =
            Loop.left (fun (k, used) ->
                
              let f = match Map.find k used with
                | None -> 0
                | Some v -> v
              in
                              
              if (f = 1) then (Left (k + 1), used)
              else (Right k, used)
            ) k used;
          in
          
          let t, _ =
            Loop.left (fun (t, use) ->

              let f = match Map.find t use with
                | None -> 0
                | Some v -> v
              in
                              
              if (f = 1) then (Left (t + 1), use)
              else (Right t, use)
            ) t use;
          in
          (*
          let ijkt = 
            if (i = 4 & j = 3) then
              (num + 1, num + 1, num + 1, num + 1)
            else
              (i, j, k, t)
          in
          let i = ijkt.(0) in
          let j = ijkt.(1) in
          let k = ijkt.(2) in
          let t = ijkt.(3) in
          *)
          (*
          let abmijbackbo =
          if (i <> 4 || j <> 3) then
          *)
          let abmijbackbo =
            if ((k < num & t < num) || (backbo.(0) = true & k < num)) then
              let tk = 
                if (t >= num) then
                  (0, k + 1)
                else
                  (t, k)
              in
              
              let t = tk.(0) in
              let k = tk.(1) in
              
              let abmktij = (abmkt.(0), abmkt.(1), abmkt.(2), k, t, i, j) in
              let bb = false in
              
              let abmktij, bb = 
                Loop.left (fun (abmktij, _) ->
                  
                  let k = abmktij.(3) in
                  let k, _ = 
                    Loop.left (fun (k, used) ->

                      let f = match Map.find k used with
                        | None -> 0
                        | Some v -> v
                      in
                            
                    if (f = 1) then (Left (k + 1), used)
                    else (Right k, used)
                    ) k used;
                  in
                  
                  let t = abmktij.(4) in
                  let t, _ = 
                    Loop.left (fun (t, use) ->

                      let f = match Map.find t use with
                        | None -> 0
                        | Some v -> v
                      in
                            
                    if (f = 1) then (Left (t + 1), use)
                    else (Right t, use)
                    ) t use;
                  in
                  (*
                  let abmktbb = 
                    (abmktij.(0), abmktij.(1), abmktij.(2), k, t, bb)
                  in
                  *)
                  let abmktbb = 
                    if (t >= num || k >= num) then
                      (abmktij.(0), abmktij.(1), abmktij.(2), k + 1, 0, false)
                    else
                      let s = k * num + t in
                      let f = match Map.find s abmktij.(2) with
                        | None -> 0
                        | Some v -> v
                      in
                      
                      let abmktbb =
                        if (f <> 1) then
                          let m = Map.add s 1 abmktij.(2) in
                          let s = abmktij.(5) * num + abmktij.(6) in
                          let a = Map.add s k abmktij.(0) in
                          let b = Map.add s t abmktij.(1) in
                          (a, b, m, k, t, true)
                        else
                          let t = t + 1 in
                          let tk = (t, k) in
                          let tk = 
                            if (t >= num) then
                              (0, k + 1)
                            else
                              (tk)
                          in
                          (abmktij.(0), abmktij.(1), abmktij.(2), tk.(1), tk.(0), false)
                      in
                      
                      abmktbb
                  in
                  
                  let bb = abmktbb.(5) in
                  let abmktij = 
                    (abmktbb.(0), abmktbb.(1), abmktbb.(2), abmktbb.(3), abmktbb.(4), abmktij.(5), abmktij.(6)) 
                  in
                  
                  if (abmktij.(3) < num & bb = false) then (Left abmktij, bb)
                  else (Right abmktij, bb)
                ) abmktij bb;
              in
              
              let ijbackbo = 
                if (bb = true) then
                  let ij = 
                    if (abmktij.(6) < (num - 1)) then
                      (abmktij.(5), abmktij.(6) + 1)
                    else
                      (abmktij.(5) + 1, 0)
                  in
                  (ij.(0), ij.(1), false, backbo.(1))
                else
                  let ij = 
                    if (abmktij.(6) > 0) then
                      (abmktij.(5), abmktij.(6) - 1)
                    else
                      (abmktij.(5) - 1, num - 1)
                  in
                  (ij.(0), ij.(1), true, 1)
              in
              (abmktij.(0), abmktij.(1), abmktij.(2), ijbackbo.(0), ijbackbo.(1), ijbackbo.(2), ijbackbo.(3))
            
            else
              let b_o =
                if (backbo.(0) = false) then
                  let b_o = 
                    if (k >= num & t >= num) then
                      2
                    else
                      let b_o = 
                        if (k >= num) then
                          1
                        else
                          0
                      in
                      b_o
                  in
                  b_o
                else
                  0
              in
              
              let ij = 
                if (j > 0) then
                  (i, j - 1)
                else
                  (i - 1, num - 1)
              in
              
              (abmkt.(0), abmkt.(1), abmkt.(2), ij.(0), ij.(1), true, b_o)
          in
            (*
            abmijbackbo
            
          else
            (abmkt.(0), abmkt.(1), abmkt.(2), i, j, backbo.(0), backbo.(1))
            
          in
          *)
          
          
          (*
          (used, use, abmij.(2), abmij.(3), abmij.(4), backbo.(0), backbo.(1))
          *)          

          abmijbackbo
      in
      (*
      let ij = 
        if (abmijbackbo.(3) = 4 & abmijbackbo.(4) = 1) then
          (num, num)
        else
          (abmijbackbo.(3), abmijbackbo.(4))
      in
      
      
      let abmij = (abmijbackbo.(0), abmijbackbo.(1), abmijbackbo.(2), ij.(0), ij.(1)) in
      *)
      
      let abmij = (abmijbackbo.(0), abmijbackbo.(1), abmijbackbo.(2), abmijbackbo.(3), abmijbackbo.(4)) in
      
      let backbo = (abmijbackbo.(5), abmijbackbo.(6)) in
        
      if (abmij.(3) < (num - 1) || abmij.(4) < (num - 1)) then (Left abmij, backbo)
      else (Right abmij, backbo)
    ) abmij backbo;
  in
  
  let m = Map [] in
  let abij = (abmij.(0), abmij.(1), 0, 0) in
  
  let m, _ = 
    Loop.left (fun (m, abij) ->
      let s = abij.(2) * num + abij.(3) in
      let t = match Map.find s abij.(0) with
        | None -> 0
        | Some v -> v
      in
      let k = match Map.find s abij.(1) with
        | None -> 0
        | Some v -> v
      in
      let p = t * num + k + 1 in
      
      let m = Map.add s p m in
      
      let ij = 
        if (abij.(3) < (num - 1)) then
          (abij.(2), abij.(3) + 1)
        else
          (abij.(2) + 1, 0)
      in
           
      let abij = (abij.(0), abij.(1), ij.(0), ij.(1)) in
                        
      if (abij.(2) < num) then (Left m, abij)
      else (Right m, abij)
    ) m abij;
  in
  
  ( [], m)