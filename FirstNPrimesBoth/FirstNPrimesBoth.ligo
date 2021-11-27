type storage = int

type parameter =
  Primes of int

type return = operation list * storage

type pr = (int, int) map

let access (k, m : int * pr) : int =
  match Map.find_opt k m with
    Some value -> value
  | None -> (failwith "No value associated." : int)

let firstnprimes (num : int) : storage = 
    let m : pr = Map.literal [0, 2] in
    let p : int = 1 in
    let i : int = 3 in
    let c : int = 0 in
    
    let rec while_one (m, num, p, i : pr * int * int * int) : int =
        let rec while_two (m, mz, i, c : pr * int * int * int) : int = 
            if i mod mz <> 0n && mz * mz <= i then while_two(m, access(c, m), i, c + 1) else mz
        in

        let mz : int = while_two (m, 2, i, 0) in

        let m : pr = if i mod mz <> 0n then Map.add p i m else m in
        let p : int = if i mod mz <> 0n then p + 1 else p in

        if p < num then while_one(m, num, p, i + 2) else i
    in

    let i : int =
        if p < num then while_one(m, num, p, i) else i
    in

    let i : int = if num <= 1 then 2 else i in
    i
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),
 (match action with
   Primes (n) -> firstnprimes (n))
