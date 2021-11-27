type storage = (int, int) map

type parameter =
  NQueens of int

type return = operation list * storage

type q = (int, int) map
type t = q * int * bool

let access (k, m : int * q) : int =
  match Map.find_opt k m with
    Some value -> value
  | None -> 0

let n_queens (n : int) : storage = 
    let m : q = Map.empty in
    let used : q = Map.empty in
    let count : int = 0 in
    let k : int = 0 in
    let back : bool = false in

    let count =
        if (n <= 0 || n = 2 || n = 3) then n else count
    in
    
    let rec main_while(used, k, t, n : q * int * t * int) : q = 
        let (_, count, _) : t = t in

        let t = 
            if count = 0 then (
                let (m, count, back) : t = t in

                let t = 
                    if back = false then (
                        let m = Map.add count 0 m in
                        (m, count, back)
                    ) else (
                        let mz = access(count, m) in
                        let mz = mz + 1 in
                        let m = Map.add count mz m in

                        let back = false in
                        (m, count, back)
                    )
                in

                let (m, count, back) = t in
                
                (m, count + 1, back)
            ) else (
                let (m, count, back) : t = t in

                let rec while_diagonals(m, n, k, count, used : q * int * int * int * q) : q =
                    let v = access(k, m) in
                    let used = Map.add v 1 used in
                    let d = count - k in
                    let vd = v + d in
                    let dv = v - d in

                    let used =
                        if v + d < n then Map.add vd 1 used else used
                    in

                    let used =
                        if v - d >= 0 then Map.add dv 1 used else used
                    in

                    let k = k + 1 in

                    if k < count then while_diagonals(m, n, k, count, used) else used
                in

                let used =
                    if k < count then while_diagonals(m, n, k, count, used) else used
                in

                let used =
                    if back = true then (
                        let v = access(count, m) in
                        let k = 0 in
                        
                        let rec while_fill(used, k, v : q * int * int) : q =
                            let used = Map.add k 1 used in

                            let k = k + 1 in

                            if k < v + 1 then while_fill(used, k, v) else used
                        in

                        if k < v + 1 then while_fill(used, k, v) else used
                    ) else used
                in

                let back =
                    if back = true then false else back
                in

                let k = 0 in

                let rec while_find_empty(used, k : q * int) : int =
                    let k = k + 1 in

                    if access(k, used) = 1 then while_find_empty(used, k) else k
                in

                let k =
                    if access(k, used) = 1 then while_find_empty(used, k) else k
                in

                let back = if k >= n then true else back in
                let m = if k >= n then m else Map.add count k m in
                let count = if k >= n then count - 1 else count + 1 in

                (m, count, back)
            )
        in

        let (m, count, back) = t in

        if count < n then main_while(used, k, t, n) else m
    in
    
    let t : t = (m, count, back) in

    let m =
        if count < n then main_while(used, k, t, n) else m 
    in

    m
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),
 (match action with
   NQueens (n) -> n_queens (n))
