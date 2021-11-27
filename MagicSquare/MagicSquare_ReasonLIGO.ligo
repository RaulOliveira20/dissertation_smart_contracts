type storage = map (int, int);

type parameter =
 MagicSquare (int)

type return = (list (operation), storage);

type q = map (int, int);

let access = ((k, m) : (int, q)) : int => {
  switch (Map.find_opt (k, m)) {
  | Some value => value
  | None => 0
  }
}

let accessN = ((k, m, n) : (int, q, int)) : int => {
  switch (Map.find_opt (k, m)) {
  | Some value => value
  | None => n
  }
}

let magicsquare = ((n) : (int)) : storage => 
    let a : q = Map.empty;
    let b : q = Map.empty;
    let used : q = Map.empty;
    let m : q = Map.empty;
    let i : int = 0;
    let j : int = 0;
    let s : int = 0;
    let k : int = 0;
    let t : int = 0;
    let p : int = 0;
    let back : bool = false;
    let b_o : int = 0;
    let bb : bool = false;

    let t = ((n - 1) / 2);
    let k = (n - 1);
    
    let rec diagonal_one = ((a, b, n, i, j, k) : (q, q, int, int, int, int)) : (q, q) =>

        let t = if (n mod 2 == 0n) {i;} else {t};

        let s = i * n + j;
        let p = i * n + k;
        let a = Map.add(s, t, a);
        let b = Map.add(p, t, b);
        let i = i + 1;
        let j = j + 1;
        let k = k - 1;

        if (i < n) {diagonal_one(a, b, n, i, j, k);} else {(a, b)};
    
    
    let a, b = if (i < n) {diagonal_one(a, b, n, i, j, k);} else {(a, b)};
    
    
    let i = n - 1;
    let t = (n - 1) / 2;
    
    let m, used = 
        if (n mod 2 == 1n) {
            (Map.add((t * n + t), 1, m), Map.add(t, 1, used));
        } else {(m, used)};

    
    let rec diagonal_two = ((a, b, used, m, n, i, j, p) : (q, q, q, q, int, int, int, int)) : (q, q, q) =>
        let i, j, p = if ((i == j) && (n != 1)) {(i - 1, j + 1, p + 1)} else {(i, j, p)};

        let s = i * n + j;
        let k = 0;
        
        let k = 
            if (n mod 2 == 1n) {
                let rec while_dt_odd = ((used, k) : (q, int)) : int =>
                    let k = k + 1;
                    let v = access(k, used);

                    if (v == 1) {while_dt_odd(used, k);} else {k};

                let v = access(k, used);
                let k = if (v == 1) {while_dt_odd(used, k);} else {k};
                
                k
            } else {
                let rec while_dt_even = ((used, k, i, j) : (q, int, int, int)) : int =>
                    let k = k + 1;
                    let v = access(k, used);

                    if ((v == 1) || (k == i) || (k == j)) {while_dt_even(used, k, i, j);} else {k};
                
                let v = access(k, used);
                let k = if ((v == 1) || (k == i) || (k == j)) {while_dt_even(used, k, i, j);} else {k};
                
                k
            };
        
        
        let a = Map.add(s, k, a);
        let used = Map.add(k, 1, used);
        
        let s = k * n + access(s, b);
        let m = Map.add(s, 1, m);

        let s = p * n + j;
        let b = Map.add(s, k, b);

        let s = access(s, a) * n + k;
        let m = Map.add(s, 1, m);

        let i = i - 1;
        let j = j + 1;
        let p = p + 1;
        

        if (j < n) {diagonal_two(a, b, used, m, n, i, j, p);} else {(a, b, m)};
    

    let a, b, m = if (j < n) {diagonal_two(a, b, used, m, n, i, j, p);} else {(a, b, m)};
    
    let used : q = Map.empty;
    let i = 0;
    let j = 0;
    let k = 0;
    
    let rec backtrack = ((a, b, m, n, i, j, b_o, back) : (q, q, q, int, int, int, int, bool)) : (q, q) =>
        let t = i + j;

        let a, b, m, i, j, b_o, back =
            if ((i == j) || (t == (n - 1))) {
                let i, j = 
                    if (back == false) {
                        if (j < (n - 1)) {(i, j + 1);} else {(i + 1, 0)};
                    } else { 
                        if (j > 0) {(i, j - 1)} else {(i - 1, n - 1)};
                    };
                
                (a, b, m, i, j, b_o, back)
            } else {
                let k = 0;
                let used : q = Map.empty;
                let use : q = Map.empty;

                let rec while_mark_used = ((a, b, used, use, k, i, j, n) : (q,q,q,q,int,int,int,int)) : (q, q) =>
                    let used, use =
                        if (k != j) {
                            let s = i * n + k;

                            let p = accessN(s, a, n);
                            let used = if (p != n) {Map.add(p, 1, used);} else {used};

                            let p = accessN(s, b, n);
                            let use = if (p != n) {Map.add(p, 1, use);} else {use};

                            (used, use)
                        } else {(used, use)};

                    let used, use =
                        if (k != i) {
                            let s = k * n + j;

                            let p = accessN(s, a, n);
                            let used = if (p != n) {Map.add(p, 1, used);} else {used};

                            let p = accessN(s, b, n);
                            let use = if (p != n) {Map.add(p, 1, use);} else {use};

                            (used, use)
                        } else {(used, use)};
                    

                    let k = k + 1;

                    if (k < n) {while_mark_used(a, b, used, use, k, i, j, n)} else {(used, use)};
                

                let used, use = if (k < n) {while_mark_used(a, b, used, use, k, i, j, n)} else {(used, use)};
                

                let used = 
                    if ((i == 1) && (j == 0)) {
                        let k = 0;
                        let t = if (n mod 2 == 0n) {n / 2} else {n - 1};

                        let rec while_a10 = ((used, k, t) : (q, int, int)) : q =>
                            let used = Map.add(k, 1, used);
                            let k = k + 1;

                            if (k < t) {while_a10(used, k, t);} else {used};
                        
                        if (k < t) {while_a10(used, k, t);} else {used};
                    } else {(used)};

                let use = 
                    if ((i == 1) && (j == 2)) {
                        let k = 0;
                        let t = n / 2 - 1;

                        let rec while_b12 = ((use, k, t) : (q, int, int)) : q =>
                            let use = Map.add(k, 1, use);
                            let k = k + 1;

                            if (k < t) {while_b12(use, k, t);} else {use};
                        
                        if (k < t) {while_b12(use, k, t);} else {use};
                    } else {use};

                let use = 
                    if ((i == 1) && (j == 3)) {
                        let k = 0;
                        let t = 3;

                        let rec while_b13 = ((use, k, t) : (q, int, int)) : q =>
                            let use = Map.add(k, 1, use);
                            let k = k + 1;

                            if (k < t) {while_b13(use, k, t);} else {use};
                        
                        if (k < t) {while_b13(use, k, t);} else {use};
                    } else {use};

                let a, b, m, k, t, b_o =
                    if (back == true) {
                        let s = i * n + j;
                        let k = access(s, a);
                        let t = access(s, b);
                        let a = Map.add(s, n, a);
                        let b = Map.add(s, n, b);
                        let s = k * n + t;
                        let m = Map.add(s, 0, m);

                        let k, t =
                            if (b_o == 2) {(k + 1, t + 1);} else {
                                if (b_o == 1) {(k + 1, 0);} else {(k, t + 1)};
                            };

                        (a, b, m, k, t, b_o)
                    } else {
                        (a, b, m, 0, 0, 0)
                    };

                let rec while_find_used = ((used, k) : (q, int)) : int =>
                    let k = k + 1;
                    let v = access(k, used);
                    if (v == 1) {while_find_used(used, k);} else {k};

                let v = access(k, used);
                let k = if (v == 1) {while_find_used(used, k);} else {k};

                let rec while_find_use = ((use, t) : (q, int)) : int =>
                    let t = t + 1;
                    let v = access(t, use);
                    if (v == 1) {while_find_use(use, t);} else {t};
                
                let v = access(t, use);
                let t = if (v == 1) {while_find_use(use, t);} else {t};
                

                let a, b, m, i, j, b_o, back =
                    if (((k < n) && (t < n)) || ((back == true) && (k < n))) {
                        let k, t = if (t >= n) {(k + 1, 0)} else {(k, t)};

                        let rec while_pairs = ((a,b,m,used,use,n,i,j,k,t,bb) : (q,q,q,q,q,int,int,int,int,int,bool)) : (q,q,q,bool) =>
                            let v = access(k, used);
                            let k = if (v == 1) {while_find_used(used, k)} else {k};

                            let v = access(t, use);
                            let t = if (v == 1) {while_find_use(use, t)} else {t};
                            

                            let a, b, m, k, t, bb =
                                if ((t >= n) || (k >= n)) {(a, b, m, k + 1, 0, bb)} else {
                                    let s = k * n + t;
                                    let v = access(s, m);
                                    let a, b, m, k, t, bb =
                                        if (v != 1) {
                                            let m = Map.add(s, 1, m);
                                            let s = i * n + j;
                                            let a = Map.add(s, k, a);
                                            let b = Map.add(s, t, b);

                                            (a, b, m, k, t, true)
                                        } else {
                                            let t = t + 1;
                                            if (t >= n) {(a, b, m, k + 1, 0, bb)} 
                                            else {(a, b, m, k, t, bb)};
                                        };
                                    
                                    (a, b, m, k, t, bb)
                                };
                            

                            if ((k < n) && (bb == false)) {
                            while_pairs(a,b,m,used,use,n,i,j,k,t,bb)
                            } else {(a, b, m, bb)};
                        
                        let a, b, m, bb =
                            if ((k < n) && (bb == false)) { 
                            while_pairs(a,b,m,used,use,n,i,j,k,t,false)
                            } else {(a, b, m, bb)};
                        

                        let i, j, b_o, back =
                            if (bb == true) {
                                if (j < (n - 1)) {(i, j + 1, b_o, false)}
                                else {(i + 1, 0, b_o, false)};
                            } else {
                                if (j > 0) {(i, j - 1, 1, true)}
                                else {(i - 1, n - 1, 1, true)};
                            };
                        

                        (a, b, m, i, j, b_o, back)
                    } else {
                        let b_o =
                            if (back == false) {
                                if ((k >= n) && (t >= n)) {2} else {    
                                    if (k >= n) {1} else {0};
                                };
                            } else {0};
                        

                        let i, j = if (j > 0) {(i, j - 1)} else {(i - 1, n - 1)};

                        let back = true;

                        (a, b, m, i, j, b_o, back)
                    };
                

                (a, b, m, i, j, b_o, back)
            };

        if ((i < (n - 1)) || (j < (n - 1))) {backtrack(a,b,m,n,i,j,b_o,back)} else {(a, b)};
    

    let a, b = if ((i < (n - 1)) || (j < (n - 1))) {backtrack(a,b,m,n,i,j,b_o,back)} else {(a, b)};
    
    
    let m : q = Map.empty;
    let i = 0;
    let j = 0;

    let rec final_while = ((a,b,m,n,i,j) : (q,q,q,int,int,int)) : q =>
        let s = i * n + j;
        let t = access(s, a);
        let k = access(s, b);
        let p = t * n + k + 1;
        let m = Map.add(s, p, m);

        let i, j = if (j < (n - 1)) {(i, j + 1)} else {(i + 1, 0)};
    
        if (i < n) {final_while(a,b,m,n,i,j)} else {m};
    
    let m = if (i < n) {final_while(a,b,m,n,i,j)} else {m};
    
    m;
   
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),
 (switch (action) {
  | MagicSquare (n) => magicsquare (n)}))
};
