type storage = int;

type parameter =
  Primes (int)

type return = (list (operation), storage);

let firstnprimes = ((num) : (int)) : storage => 
    let p = 1;
    let i = 3;
    let x = 3;
    
    let rec while_one = ((num, p, i, x) : (int, int, int, int)) : int =>
        let rec while_two = ((i, x) : (int, int)) : int =>
            if ((i mod x != 0n) && (x * x <= i)) {while_two(i, x + 2);} else {x;};
        
        let x = while_two(i, x);
        
        let p = if ((i mod x != 0n) || (i == 3)) {p + 1;} else {p;};

        let x = 3;
        
        if (p < num) {while_one(num, p, i + 2, x);} else {i};

    let i = if (p < num) {while_one(num, p, i, x)} else {i};

    if (num <= 1) {2} else {i};

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),
 (switch (action) {
  | Primes (n) => firstnprimes (n)}))
};
