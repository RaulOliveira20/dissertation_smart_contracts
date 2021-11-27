type storage = (int, int) map

let%entry n_queens (n : int) _ =
  
  let m = 
    if (n <= 0 or n = 2 or n = 3) then Map [] else
    
      let m = (Map [] : (int, int) map) in
      let used = (Map [] : (int, int) map) in
      let count = 0 in
      let k = 0 in
      let d = 0 in
      let v = 0 in
      let back = false in
      
      let t = (m, count, back, used, k, d, v) in
      
      let _, t = 
        Loop.left (fun (_, t) ->
          
          let m = t.(0) in
          let count = t.(1) in
          let back = t.(2) in
          let used = t.(3) in
          let k = t.(4) in
          let d = t.(5) in
          let v = t.(6) in
          
          let t = (m, count, back, used, k, d, v) in
          
          let t = 
            if count = 0 then
              let m = t.(0) in
              let count = t.(1) in
              let back = t.(2) in
              let used = t.(3) in
              let k = t.(4) in
              let d = t.(5) in
              let v = t.(6) in
              
              let t = (m, count, back, used, k, d, v) in
              
              let t =
                if back = false then
                  let m = t.(0) in
                  let count = t.(1) in
                  let back = t.(2) in
                  let used = t.(3) in
                  let k = t.(4) in
                  let d = t.(5) in
                  let v = t.(6) in
                  let m = Map.add count 0 m in
                  let t = (m, count, back, used, k, d, v) in
                  t
                else
                  let m = t.(0) in
                  let count = t.(1) in
                  let used = t.(3) in
                  let k = t.(4) in
                  let d = t.(5) in
                  let v = t.(6) in
                  
                  let x = match Map.find count m with
                    | None -> failwith ("id is not in the map", count)
                    | Some x -> x
                  in
                  
                  let x = x + 1 in
                  
                  let m = Map.add count x m in
                  
                  let back = false in
                  
                  let t = (m, count, back, used, k, d, v) in
                  t
              in
              let m = t.(0) in
              let count = t.(1) in
              let back = t.(2) in
              let used = t.(3) in
              let k = t.(4) in
              let d = t.(5) in
              let v = t.(6) in
              
              let count = count + 1 in
              
              let t = (m, count, back, used, k, d, v) in
              
              t
                
            else
              let _, t = 
                Loop.left (fun (_, t) ->
                  let m = t.(0) in
                  let count = t.(1) in
                  let back = t.(2) in
                  let used = t.(3) in
                  let k = t.(4) in
                    
                  let v = 
                    match Map.find k m with
                    | None -> failwith ("id is not in the map", k)
                    | Some v -> v
                  in
                  
                  let used = Map.add v 1 used in
                  let d = count - k in
                  
                  let used = 
                    if v + d < n then 
                      let vd = v + d in
                      Map.add vd 1 used 
                    else 
                      used
                  in
                  
                  let used = 
                    if v - d >= 0 then 
                      let dv = v - d in
                      Map.add dv 1 used 
                    else 
                      used
                  in
                  
                  let k = k + 1 in
                  let t = (m, count, back, used, k, d, v) in
                  
                if (k < count) then (Left m, t)
                else (Right m, t)
              ) m t
              in  
              
              let back = t.(2) in
              
              let t =
                if back = true then
                  let m = t.(0) in
                  let count = t.(1) in
                  let back = t.(2) in
                  let used = t.(3) in
                  let d = t.(5) in
                  
                  let v = 
                    match Map.find count m with
                    | None -> failwith ("id is not in the map", count)
                    | Some v -> v
                  in
                  
                  let k = 0 in
                  let t = (m, count, back, used, k, d, v) in
                  
                  let _, t = 
                    Loop.left (fun (_, t) ->
                      let m = t.(0) in
                      let count = t.(1) in
                      let back = t.(2) in
                      let used = t.(3) in
                      let k = t.(4) in
                      let d = t.(5) in
                      let v = t.(6) in
                      
                      let used = Map.add k 1 used in
                        
                      let k = k + 1 in
                      let t = (m, count, back, used, k, d, v) in
                      
                      if (k < v + 1) then (Left used, t)
                      else (Right used, t)
                    ) used t
                  in
                  
                  let back = false in
                  
                  let t = (t.(0), t.(1), back, t.(3), t.(4), t.(5), t.(6)) in
              
                  t
                    
                else
                  t
              in
              
              let t = (t.(0), t.(1), t.(2), t.(3), 0, t.(5), t.(6)) in
              let used = t.(3) in
              
              let _, t = 
                Loop.left (fun (_, t) ->
                    
                  let m = t.(0) in
                  let count = t.(1) in
                  let back = t.(2) in
                  let used = t.(3) in
                  let k = t.(4) in
                  let d = t.(5) in
                  let v = t.(6) in  
                  
                  let value = 
                    match Map.find k used with
                    | None -> 0
                    | Some value -> value
                  in
                  
                  let k = k + 1 in
                  let t = (m, count, back, used, k, d, v) in
                    
                if (value = 1) then (Left used, t)
                else (Right used, t)
                ) used t
              in
              
              let k = t.(4) in
              let k = k - 1 in
              let t = (t.(0), t.(1), t.(2), t.(3), k, t.(5), t.(6)) in
              
              let t =
                if k >= n then
                  let m = t.(0) in
                  let count = t.(1) in
                  let used = t.(3) in
                  let k = t.(4) in
                  let d = t.(5) in
                  let v = t.(6) in
                  
                  let back = true in
                  let count = count - 1 in
                  let t = (m, count, back, used, k, d, v) in
                  
                  t
                else
                  let m = t.(0) in
                  let count = t.(1) in
                  let back = t.(2) in
                  let used = t.(3) in
                  let k = t.(4) in
                  let d = t.(5) in
                  let v = t.(6) in
              
                  let m = Map.add count k m in
                  let count = count + 1 in
                  let t = (m, count, back, used, k, d, v) in
                  
                  t
              in
              
              let m = t.(0) in
              let count = t.(1) in
              let back = t.(2) in
              let d = t.(5) in
              let v = t.(6) in
              
              let k = 0 in
              let used = Map [] in
              let t = (m, count, back, used, k, d, v) in
                  
              t
          in
        
        if (t.(1) < n) then (Left m, t)
        else (Right m, t)
      ) m t
      in
  
    let m = t.(0) in
      
    m
  in
  
  ( [], m)
