type storage = int;

type parameter =
  Primes (int)

type return = (list (operation), storage);

type pr = map (int, int);

let access = ((k, m) : (int, pr)) : int => {
  switch (Map.find_opt (k, m)) {
  | Some value => value
  | None => (failwith ("No value associated.") : int)
  }
}

let firstnprimes = ((num) : (int)) : storage => 
    let m = Map.literal([(0, 2)]);
    let p = 1;
    let i = 3;
    let c = 0;
    
    let rec while_one = ((m, num, p, i) : (pr, int, int, int)) : int =>
        let rec while_two = ((m, mz, i, c) : (pr, int, int, int)) : int =>
            if ((i mod mz != 0n) && (mz * mz <= i)) {while_two(m, access(c, m), i, c + 1);} else {mz;};
        
        let mz = while_two(m, 2, i, 0);
        
        let m = if (i mod mz != 0n) {Map.add(p, i, m);} else {m;};
        let p = if (i mod mz != 0n) {p + 1;} else {p;};
        
        if (p < num) {while_one(m, num, p, i + 2);} else {i};

    let i = if (p < num) {while_one(m, num, p, i)} else {i};

    if (num <= 1) {2} else {i};

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),
 (switch (action) {
  | Primes (n) => firstnprimes (n)}))
};
