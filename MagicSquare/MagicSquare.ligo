type storage = (int, int) map

type parameter =
  MagicSquare of int

type return = operation list * storage

type q = (int, int) map

let access (k, m : int * q) : int =
  match Map.find_opt k m with
    Some value -> value
  | None -> 0

let accessN (k, m, n : int * q * int) : int =
  match Map.find_opt k m with
    Some value -> value
  | None -> n

let magicsquare (n : int) : storage = 
    let a : q = Map.empty in
    let b : q = Map.empty in
    let used : q = Map.empty in
    let m : q = Map.empty in
    let i : int = 0 in
    let j : int = 0 in
    let s : int = 0 in
    let k : int = 0 in
    let t : int = 0 in
    let p : int = 0 in
    let back : bool = false in
    let b_o : int = 0 in
    let bb : bool = false in

    let t = (n - 1) / 2 in
    let k = (n - 1) in
    
    let rec diagonal_one(a, b, n, i, j, k : q * q * int * int * int * int) : (q * q) = 

        let t =
            if n mod 2 = 0n then i else t
        in

        let s = i * n + j in
        let p = i * n + k in
        let a = Map.add s t a in
        let b = Map.add p t b in
        let i = i + 1 in
        let j = j + 1 in
        let k = k - 1 in

        if i < n then diagonal_one(a, b, n, i, j, k) else a, b
    in
    
    let a, b =
        if i < n then diagonal_one(a, b, n, i, j, k) else a, b
    in

    let i = n - 1 in
    let t = (n - 1) / 2 in

    let m, used = 
        if n mod 2 = 1n then (
            Map.add (t * n + t) 1 m, Map.add t 1 used
        ) else m, used
    in
    
    let rec diagonal_two(a, b, used, m, n, i, j, p : q * q * q * q * int * int * int * int) : (q * q * q) =
        let i, j, p = 
            if (i = j) && (n <> 1) then i - 1, j + 1, p + 1 else i, j, p
        in

        let s = i * n + j in
        let k = 0 in

        let k = 
            if n mod 2 = 1n then (
                let rec while_dt_odd(used, k : q * int) : int =
                    let k = k + 1 in
                    let v = access(k, used) in

                    if v = 1 then while_dt_odd(used, k) else k
                in

                let v = access(k, used) in
                let k = 
                    if v = 1 then while_dt_odd(used, k) else k
                in
                k
            ) else (
                let rec while_dt_even(used, k, i, j : q * int * int * int) : int =
                    let k = k + 1 in
                    let v = access(k, used) in

                    if (v = 1) || (k = i) || (k = j) then while_dt_even(used, k, i, j) else k
                in

                let v = access(k, used) in
                let k =                    
                    if (v = 1) || (k = i) || (k = j) then while_dt_even(used, k, i, j) else k
                in
                k
            )
        in

        let a = Map.add s k a in
        let used = Map.add k 1 used in
        
        let s = k * n + access(s, b) in
        let m = Map.add s 1 m in

        let s = p * n + j in
        let b = Map.add s k b in

        let s = access(s, a) * n + k in
        let m = Map.add s 1 m in

        let i = i - 1 in
        let j = j + 1 in
        let p = p + 1 in

        if j < n then diagonal_two(a, b, used, m, n, i, j, p) else a, b, m
    in

    let a, b, m =
        if j < n then diagonal_two(a, b, used, m, n, i, j, p) else a, b, m
    in

    let used : q = Map.empty in
    let i = 0 in
    let j = 0 in
    let k = 0 in
    
    let rec backtrack(a, b, m, n, i, j, b_o, back : q * q * q * int * int * int * int * bool) : (q * q) =
        let t = i + j in

        let a, b, m, i, j, b_o, back =
            if (i = j) || (t = (n - 1)) then (
                let i, j = 
                    if back = false then (
                        if (j < (n - 1)) then i, j + 1 else i + 1, 0
                    ) else ( 
                        if (j > 0) then i, j - 1 else i - 1, n - 1
                    )
                in
                a, b, m, i, j, b_o, back
            ) else (
                let k = 0 in
                let used : q = Map.empty in
                let use : q = Map.empty in

                let rec while_mark_used(a, b, used, use, k, i, j, n : q*q*q*q*int*int*int*int) : (q * q) =
                    let used, use =
                        if (k <> j) then (
                            let s = i * n + k in

                            let p = accessN(s, a, n) in
                            let used = 
                                if (p <> n) then Map.add p 1 used else used
                            in

                            let p = accessN(s, b, n) in
                            let use = 
                                if (p <> n) then Map.add p 1 use else use
                            in
                            used, use
                        ) else used, use
                    in
                    let used, use =
                        if (k <> i) then (
                            let s = k * n + j in

                            let p = accessN(s, a, n) in
                            let used = 
                                if (p <> n) then Map.add p 1 used else used
                            in

                            let p = accessN(s, b, n) in
                            let use = 
                                if (p <> n) then Map.add p 1 use else use
                            in
                            used, use
                        ) else used, use
                    in

                    let k = k + 1 in

                    if (k < n) then while_mark_used(a, b, used, use, k, i, j, n) else used, use
                in

                let used, use =
                    if (k < n) then while_mark_used(a, b, used, use, k, i, j, n) else used, use
                in

                let used = 
                    if (i = 1) && (j = 0) then (
                        let k = 0 in
                        let t = if (n mod 2 = 0n) then n / 2 else n - 1 in

                        let rec while_a10(used, k, t : q * int * int) : q =
                            let used = Map.add k 1 used in
                            let k = k + 1 in

                            if k < t then while_a10(used, k, t) else used
                        in
                        if k < t then while_a10(used, k, t) else used
                    ) else used
                in

                let use = 
                    if (i = 1) && (j = 2) then (
                        let k = 0 in
                        let t = n / 2 - 1 in

                        let rec while_b12(use, k, t : q * int * int) : q =
                            let use = Map.add k 1 use in
                            let k = k + 1 in

                            if k < t then while_b12(use, k, t) else use
                        in
                        if k < t then while_b12(use, k, t) else use
                    ) else use
                in

                let use = 
                    if (i = 1) && (j = 3) then (
                        let k = 0 in
                        let t = 3 in

                        let rec while_b13(use, k, t : q * int * int) : q =
                            let use = Map.add k 1 use in
                            let k = k + 1 in

                            if k < t then while_b13(use, k, t) else use
                        in
                        if k < t then while_b13(use, k, t) else use
                    ) else use
                in

                let a, b, m, k, t, b_o =
                    if back = true then (
                        let s = i * n + j in
                        let k = access(s, a) in
                        let t = access(s, b) in
                        let a = Map.add s n a in
                        let b = Map.add s n b in
                        let s = k * n + t in
                        let m = Map.add s 0 m in

                        let k, t =
                            if b_o = 2 then k + 1, t + 1 else (
                                if b_o = 1 then k + 1, 0 else k, t + 1
                            )
                        in

                        a, b, m, k, t, b_o
                    ) else (
                        a, b, m, 0, 0, 0
                    )
                in

                let rec while_find_used(used, k : q * int) : int =
                    let k = k + 1 in
                    let v = access(k, used) in
                    if (v = 1) then while_find_used(used, k) else k
                in

                let v = access(k, used) in
                let k =
                    if (v = 1) then while_find_used(used, k) else k
                in

                let rec while_find_use(use, t : q * int) : int =
                    let t = t + 1 in
                    let v = access(t, use) in
                    if (v = 1) then while_find_use(use, t) else t
                in

                let v = access(t, use) in
                let t =
                    if (v = 1) then while_find_use(use, t) else t
                in

                let a, b, m, i, j, b_o, back =
                    if ((k < n) && (t < n)) || ((back = true) && (k < n)) then (
                        let k, t = if t >= n then k + 1, 0 else k, t in

                        let rec while_pairs(a,b,m,used,use,n,i,j,k,t,bb : q*q*q*q*q*int*int*int*int*int*bool) : (q*q*q*bool) =
                            let v = access(k, used) in
                            let k =
                                if (v = 1) then while_find_used(used, k) else k
                            in

                            let v = access(t, use) in
                            let t =
                                if (v = 1) then while_find_use(use, t) else t
                            in

                            let a, b, m, k, t, bb =
                                if (t >= n) || (k >= n) then a, b, m, k + 1, 0, bb else (
                                    let s = k * n + t in
                                    let v = access(s, m) in
                                    let a, b, m, k, t, bb =
                                        if (v <> 1) then (
                                            let m = Map.add s 1 m in
                                            let s = i * n + j in
                                            let a = Map.add s k a in
                                            let b = Map.add s t b in

                                            a, b, m, k, t, true
                                        ) else (
                                            let t = t + 1 in
                                            if t >= n then a, b, m, k + 1, 0, bb 
                                            else a, b, m, k, t, bb
                                        )
                                    in
                                    a, b, m, k, t, bb
                                )
                            in

                            if (k < n) && (bb = false) then
                            while_pairs(a,b,m,used,use,n,i,j,k,t,bb)
                            else a, b, m, bb
                        in
                        let a, b, m, bb =
                            if (k < n) && (bb = false) then 
                            while_pairs(a,b,m,used,use,n,i,j,k,t,false)
                            else a, b, m, bb
                        in

                        let i, j, b_o, back =
                            if bb = true then (
                                if (j < (n - 1)) then i, j + 1, b_o, false
                                else i + 1, 0, b_o, false
                            ) else (
                                if j > 0 then i, j - 1, 1, true
                                else i - 1, n - 1, 1, true
                            )
                        in

                        a, b, m, i, j, b_o, back
                    ) else (
                        let b_o =
                            if back = false then (
                                if (k >= n) && (t >= n) then 2 else (    
                                    if k >= n then 1 else 0
                                )
                            ) else 0
                        in

                        let i, j =
                            if j > 0 then i, j - 1 else i - 1, n - 1
                        in

                        let back = true in

                        a, b, m, i, j, b_o, back
                    )
                in

                a, b, m, i, j, b_o, back
            )
        in

        if (i < (n - 1)) || (j < (n - 1)) then backtrack(a,b,m,n,i,j,b_o,back) else a, b
    in

    let a, b =
        if (i < (n - 1)) || (j < (n - 1)) then backtrack(a,b,m,n,i,j,b_o,back) else a, b
    in
    
    let m : q = Map.empty in
    let i = 0 in
    let j = 0 in

    let rec final_while(a,b,m,n,i,j : q*q*q*int*int*int) : q =
        let s = i * n + j in
        let t = access(s, a) in
        let k = access(s, b) in
        let p = t * n + k + 1 in
        let m = Map.add s p m in

        let i, j = 
            if j < (n - 1) then i, j + 1 else i + 1, 0
        in
        if i < n then final_while(a,b,m,n,i,j) else m
    in
    let m =
        if i < n then final_while(a,b,m,n,i,j) else m
    in

    m
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),
 (match action with
   MagicSquare (n) -> magicsquare (n))
