type storage is map (int, int)

type parameter is
  MagicSquare of int

type return is list (operation) * storage

type q is map (int, int)

function access (const k : int; const m : q) : int is
  case m[k] of
    Some (val) -> val
  | None -> 0
  end

function accessN (const k : int; const m : q; const n : int) : int is
  case m[k] of
    Some (val) -> val
  | None -> n
  end

function magicsquare (const n : int) : storage is 
  block {
      var a : q := map [];
      var b : q := map [];
      var used : q := map [];
      var use : q := map [];
      var m : q := map [];
      var i : int := 0;
      var j : int := 0;
      var s : int := 0;
      var k : int := 0;
      var t : int := 0;
      var p : int := 0;
      var back : bool := False;
      var b_o : int := 0;
      var bb : bool := False;
      
      t := (n - 1) / 2;
      k := (n - 1);

      while i < n block {
        if n mod 2 = 0n then {
          t := i;
        } else skip;

        s := i * n + j;
        p := i * n + k;
        a[s] := t;
        b[p] := t;
        i := i + 1;
        j := j + 1;
        k := k - 1;
      };

      i := n - 1;
      j := 0;
      p := 0;
      k := 0;
      t := (n - 1) / 2;

      if n mod 2 = 1n then {
        used[t] := 1;
        m[t * n + t] := 1;
      } else skip;

      while j < n block {
          if (i = j) and (n =/= 1) then {
              i := i - 1;
              j := j + 1;
              p := p + 1;
          } else skip;

          s := i * n + j;

          if n mod 2 = 1n then {
              for z := 0 to n block {
                  if access(k, used) = 1 then
                    k := k + 1;
                  else
                    z := n;
              }
          } else {
              for z := 0 to n block {
                  if (access(k, used) = 1) or (k = i) or (k = j) then
                    k := k + 1;
                  else
                    z := n;
              }
          };

          a[s] := k;
          used[k] := 1;

          s := k * n + access(s, b);
          m[s] := 1;

          s := p * n + j;
          b[s] := k;

          s := access(s, a) * n + k;
          m[s] := 1;

          i := i - 1;
          j := j + 1;
          p := p + 1;
          k := 0;
      };

      used := map [0->0];
      i := 0;
      j := 0;
      k := 0;

      while (i < (n - 1)) or (j < (n - 1)) block {
          t := i + j;

          if (i = j) or (t = (n - 1)) then {
              if (back = False) then {
                  if (j < (n - 1)) then
                    j := j + 1;
                  else {
                    i := i + 1;
                    j := 0;
                  };
              } else {
                  if (j > 0) then 
                    j := j - 1;
                  else {
                    i := i - 1;
                    j := n - 1;
                  };
              };
          } else {
              k := 0;

              for z := 0 to n block {
                  if k < n then {
                      if k =/= j then {
                          s := i * n + k;

                          p := accessN(s, a, n);
                          if p =/= n then
                            used[p] := 1;
                          else skip;

                          p := accessN(s, b, n);
                          if p =/= n then
                            use[p] := 1;
                          else skip;
                      } else skip;

                      if k =/= i then {
                          s := k * n + j;

                          p := accessN(s, a, n);
                          if p =/= n then
                            used[p] := 1;
                          else skip;

                          p := accessN(s, b, n);
                          if p =/= n then
                            use[p] := 1;
                          else skip;
                      } else skip;

                      k := k + 1;
                  }
                  else z := n;
              };

              if (i = 1) and (j = 0) then {
                  k := 0;

                  if (n mod 2 = 0n) then
                    t := n / 2;
                  else
                    t := n - 1;

                  for z := 0 to t block {
                      if (k < t) then {
                          used[k] := 1;
                          k := k + 1;
                      } else z := t;
                  }
              } else skip;

              if (i = 1) and (j = 2) then {
                  k := 0;
                  t := n / 2 - 1;

                  for z := 0 to t block {
                      if (k < t) then {
                          use[k] := 1;
                          k := k + 1;
                      } else z := t;
                  }
              } else skip;

              if (i = 1) and (j = 3) then {
                  k := 0;
                  t := 3;

                  for z := 0 to t block {
                      if (k < t) then {
                          use[k] := 1;
                          k := k + 1;
                      } else z := t;
                  }
              } else skip;

              if (back = True) then {
                  s := i * n + j;
                  k := access(s, a);
                  t := access(s, b);
                  a[s] := n;
                  b[s] := n;

                  s := k * n + t;
                  m[s] := 0;

                  if (b_o = 2) then {
                      k := k + 1;
                      t := t + 1;
                  } else {
                      if (b_o = 1) then {
                          k := k + 1;
                          t := 0;
                      } else t := t + 1;
                  };
              } else {
                  k := 0;
                  t := 0;
                  b_o := 0;
              };

              for z := 0 to n block {
                  if access(k, used) = 1 then
                    k := k + 1;
                  else
                    z := n;
              };

              for z := 0 to n block {
                  if access(t, use) = 1 then
                    t := t + 1;
                  else
                    z := n;
              };

              if ((k < n) and (t < n)) or ((back = True) and (k < n)) then {
                  if (t >= n) then {
                      t := 0;
                      k := k + 1;
                  } else skip;
              
                  for zz := 0 to (n*n) block {
                      if (k < n) and (bb = False) then {
                        for z := 0 to n block {
                            if access(k, used) = 1 then
                                k := k + 1;
                            else
                                z := n;
                        };

                        for z := 0 to n block {
                            if access(t, use) = 1 then
                                t := t + 1;
                            else
                                z := n;
                        };

                        if (t >= n) or (k >= n) then {
                            k := k + 1;
                            t := 0;
                        } else {
                            s := k * n + t;
                            if (access(s, m) =/= 1) then {
                                m[s] := 1;
                                s := i * n + j;
                                a[s] := k;
                                b[s] := t;

                                bb := True;
                            } else {
                                t := t + 1;

                                if (t >= n) then {
                                    k := k + 1;
                                    t := 0;
                                } else skip;
                            };
                        };

                      } else zz := n*n;
                  };

                  if (bb = True) then {
                      if (j < (n - 1)) then
                        j := j + 1;
                      else {
                        i := i + 1;
                        j := 0;
                      };

                      bb := False;
                      back := False;
                  } else {
                      if (j > 0) then
                        j := j - 1;
                      else {
                        i := i - 1;
                        j := n - 1;
                      };

                      back := True;
                      b_o := 1;
                  }
              } else {
                  if (back = False) then {
                      if (k >= n) and (t >= n) then
                        b_o := 2
                      else {
                        if (k >= n) then
                            b_o := 1
                        else
                            b_o := 0
                      };
                  } else b_o := 0;

                  if (j > 0) then 
                    j := j - 1;
                  else {
                    i := i - 1;
                    j := n - 1;
                  };

                  back := True;
              };

              k := 0;
              used := map[0->0];
              use := map[0->0];
          };

      };

      m := map[0->0];
      i := 0;
      j := 0;

      while i < n block {
          s := i * n + j;
          t := access(s, a);
          k := access(s, b);
          p := t * n + k + 1;
          m[s] := p;

          if j < (n - 1) then
            j := j + 1;
          else {
            i := i + 1;
            j := 0;
          };
      }

  } with m
   
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),
  case action of
    MagicSquare (n) -> magicsquare (n)
  end)
