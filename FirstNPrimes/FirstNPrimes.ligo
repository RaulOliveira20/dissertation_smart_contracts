type storage is int

type parameter is
  Primes of int

type return is list (operation) * storage

function firstnprimes (const num : int) : storage is 
  block {
      var p : int := 1;
      var i : int := 3;
      var x : int := 3;

      while p < num block {
        while (i mod x =/= 0n) and (x * x <= i) block {
            x := x + 2;
        };

        if i mod x =/= 0n or i = 3 then p := p + 1 else skip;

        x := 3;

        i := i + 2;
      };

      if num <= 1 then i := 2 else i := i - 2

  } with i
   
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),
  case action of
    Primes (n) -> firstnprimes (n)
  end)