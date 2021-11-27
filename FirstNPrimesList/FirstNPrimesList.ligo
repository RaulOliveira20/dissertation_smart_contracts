type storage = int

type parameter =
  Primes of int

type return = operation list * storage

let firstnprimes (num : int) : storage = 
    let p : int = 1 in
    let i : int = 3 in
    let c : int = 0 in
    let s : int set = Set.empty in
    let s : int set = Set.add 2 s in

    let rec while_one (num, p, i, c, s : int * int * int * int * int set) : int =
        let sum (c, e : int * int) : int = if i mod e = 0n then 1 else c in
        let c : int = Set.fold sum s c in

        let s : int set = if c = 0 then Set.add i s else s in
        let p : int = if c = 0 then p + 1 else p in

        let c : int = 0 in

        if p < num then while_one(num, p, i + 2, c, s) else i
    in

    let i : int =
        if p < num then while_one(num, p, i, c, s) else i
    in

    let i : int = if num <= 1 then 2 else i in
    i
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),
 (match action with
   Primes (n) -> firstnprimes (n))
