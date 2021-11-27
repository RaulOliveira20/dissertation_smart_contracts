type storage = int

type parameter =
  Primes of int

type return = operation list * storage

let firstnprimes (num : int) : storage = 
    let p : int = 1 in
    let i : int = 3 in
    let x : int = 3 in
    
    let rec while_one (num, p, i, x : int * int * int * int) : int =
        let rec while_two (i, x : int * int) : int = 
            if i mod x <> 0n && x * x <= i then while_two(i, x + 2) else x
        in
        let x : int = while_two (i, x) in

        let p : int = if i mod x <> 0n || i = 3 then p + 1 else p in

        let x : int = 3 in

        if p < num then while_one(num, p, i + 2, x) else i
    in

    let i : int =
        if p < num then while_one(num, p, i, x) else i
    in

    let i : int = if num <= 1 then 2 else i in
    i
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),
 (match action with
   Primes (n) -> firstnprimes (n))
