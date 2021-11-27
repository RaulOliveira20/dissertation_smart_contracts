type storage is map (int, int)

type parameter is
  NQueens of int

type return is list (operation) * storage

type q is map (int, int)

function access (const k : int; const m : q) : int is
  case m[k] of
    Some (val) -> val
  | None -> 0
  end

function n_queens (const n : int) : storage is 
  block {
      var m : q := map [];
      var used : q := map [];
      var count : int := 0;
      var k : int := 0;
      var d : int := 0;
      var v : int := 0;
      var back : bool := False;

      if n <= 0 or n = 2 or n = 3 then
        count := n
      else {
        skip;
      };

      while count < n block {
        if count = 0 then {
            if back = False then
                m[count] := 0;
            else {
                v := access(count, m);
                m[count] := v + 1;

                back := False
            };

            count := count + 1
        }
        else {
            for i := k to count-1 block {
                v := access(i, m);
                used[v] := 1;
                d := count - i;

                if v + d < n then
                    used[v + d] := 1
                else skip;

                if v - d >= 0 then
                    used[v - d] := 1
                else skip;
            };

            if back = True then {
                v := access(count, m);
                for i := 0 to v block {
                    used[i] := 1;
                };

                back := False

            } else skip;
        
            k := 0;

            for i := 0 to n-1 block {
                if access(k, used) = 1 then
                    k := k + 1;
                else
                    i := n-1;
            };

            if k >= n then {
                back := True;
                count := count - 1
            }
            else {
                m[count] := k;
                count := count + 1
            };

            k := 0;
            used := map [0->0]
        }
      }

  } with m
   
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),
  case action of
    NQueens (n) -> n_queens (n)
  end)
