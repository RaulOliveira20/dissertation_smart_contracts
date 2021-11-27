type storage = int;

type parameter =
  Primes (int)

type return = (list (operation), storage);

let firstnprimes = ((num) : (int)) : storage => 
    let p = 1;
    let i = 3;
    let c = 0;
    let s : set (int) = Set.empty;
    let s : set (int) = Set.add (2, s);
    
    let rec while_one = ((num, p, i, c, s) : (int, int, int, int, set (int))) : int =>
        let sum = ((c, e) : (int, int)) : int => if (i mod e == 0n) {1} else {c};
        let c : int = Set.fold (sum, s, c);

        let s : set (int) = if (c == 0) {Set.add (i, s)} else {s};
        let p : int = if (c == 0) {p + 1} else {p};

        let c = 0;
        
        if (p < num) {while_one(num, p, i + 2, c, s);} else {i};

    let i = if (p < num) {while_one(num, p, i, c, s)} else {i};

    if (num <= 1) {2} else {i};

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),
 (switch (action) {
  | Primes (n) => firstnprimes (n)}))
};
