type storage is int

type parameter is
  Primes of int

type return is list (operation) * storage

function firstnprimes (const num : int) : storage is 
  block {
      var p : int := 1;
      var i : int := 3;
      var c : int := 0;
      var s : set (int) := set [2];

      while p < num block {
        for x in set s block {
            if i mod x = 0n then c := 1 else skip;
        };

        if c = 0 then
            block {
                s := Set.add (i, s);
                p := p + 1
            }
        else skip;

        c := 0;

        i := i + 2;
      };

      if num <= 1 then i := 2 else i := i - 2

  } with i
   
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),
  case action of
    Primes (n) -> firstnprimes (n)
  end)
