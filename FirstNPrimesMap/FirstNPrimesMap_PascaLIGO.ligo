type storage is int

type parameter is
  Primes of int

type return is list (operation) * storage

type pr is map (int, int);

function access (const k : int; const m : pr) : int is
  case m[k] of
    Some (val) -> val
  | None -> (failwith ("No value associated.") : int)
  end

function firstnprimes (const num : int) : storage is 
  block {
      var m : pr := map [0 -> 2];
      var i : int := 3;
      var p : int := 1;
      var c : int := 0;
      var d : int := 0;

      var mz : int := access(0, m);

      while p < num block {
        while (c < p) block {
            if i mod mz = 0n then {
                d := 1;
            }
            else skip;

            mz := access(c, m);

            c := c + 1;
        };

        if d = 0 then {
            m[p] := i;
            p := p + 1; 
        }
        else skip;

        var mz := access(0, m);

        c := 0;
        d := 0;

        i := i + 2;
      };

      if num <= 1 then i := 2 else i := i - 2

  } with i
   
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),
  case action of
    Primes (n) -> firstnprimes (n)
  end)
