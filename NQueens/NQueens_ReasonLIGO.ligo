type storage = map (int, int);

type parameter =
 NQueens (int)

type return = (list (operation), storage);

type q = map (int, int);
type t = (q, int, bool);

let access = ((k, m) : (int, q)) : int => {
  switch (Map.find_opt (k, m)) {
  | Some value => value
  | None => 0
  }
}

let n_queens = ((n) : (int)) : storage => 
    let m : q = Map.empty;
    let used : q = Map.empty;
    let count = 0;
    let k = 0;
    let back = false;

    let count = if (n <= 0 || n == 2 || n == 3) {n;} else {count;};

    let rec main_while = ((used, k, t, n) : (q, int, t, int)) : q => 
        let (_, count, _) : t = t;

        let t = 
            if (count == 0) {
                let (m, count, back) : t = t;

                let t = 
                    if (back == false) {
                        let m = Map.add(count, 0, m);
                        (m, count, back);
                    } else {
                        let mz = access(count, m);
                        let mz = mz + 1;
                        let m = Map.add(count, mz, m);

                        let back = false;
                        (m, count, back);
                    };

                let (m, count, back) = t;
                
                (m, count + 1, back);
            } else {
                let (m, count, back) : t = t;

                let rec while_diagonals = ((m, n, k, count, used) : (q, int, int, int, q)) : q =>
                    let v = access(k, m);
                    let used = Map.add(v, 1, used);
                    let d = count - k;
                    let vd = v + d;
                    let dv = v - d;

                    let used = if (v + d < n) {Map.add(vd, 1, used);} else {used;};
                    
                    let used = if (v - d >= 0) {Map.add(dv, 1, used);} else {used;};

                    let k = k + 1;

                    if (k < count) {while_diagonals(m, n, k, count, used);} else {used;};

                let used = if (k < count) {while_diagonals(m, n, k, count, used);} else {used;};

                let used =
                    if (back == true) {
                        let v = access(count, m);
                        let k = 0;
                        
                        let rec while_fill = ((used, k, v) : (q, int, int)) : q =>
                            let used = Map.add(k, 1, used);

                            let k = k + 1;

                            if (k < v + 1) {while_fill(used, k, v);} else {used;};

                        if (k < v + 1) {while_fill(used, k, v);} else {used;};
                    } else {used;};

                let back = if (back == true) {false;} else {back;};

                let k = 0;

                let rec while_find_empty = ((used, k) : (q, int)) : int =>
                    let k = k + 1;

                    if (access(k, used) == 1) {while_find_empty(used, k);} else {k;};

                let k = if (access(k, used) == 1) {while_find_empty(used, k);} else {k;};

                let back = if (k >= n) {true;} else {back;};
                let m = if (k >= n) {m;} else {Map.add(count, k, m);};
                let count = if (k >= n) {count - 1;} else {count + 1;};

                (m, count, back);
            };

        let (m, count, back) = t;

        if (count < n) {main_while(used, k, t, n);} else {m;};
    
    let t : t = (m, count, back);

    let m = if (count < n) {main_while(used, k, t, n);} else {m;};

    m;
   
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),
 (switch (action) {
  | NQueens (n) => n_queens (n)}))
};
