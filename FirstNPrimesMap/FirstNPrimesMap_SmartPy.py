import smartpy as sp

class FirstNPrimesMap(sp.Contract):
    def __init__(self):
        self.init(last_prime = sp.int(0), primes = {0 : 2})
    
    @sp.entry_point
    def n_primes(self, num):
        p = sp.local("p", 1)
        i = sp.local("i", 3)
        c = sp.local("c", 0)
        d = sp.local("d", 0)
        n = sp.local("n", num)
        
        sp.while p.value < n.value:
            
            sp.while (c.value < p.value):
                sp.if (i.value % self.data.primes[c.value] == 0):
                    d.value = 1
                c.value += 1

            sp.if (d.value == 0):
                self.data.primes[p.value] = i.value
                p.value += 1

            c.value = 0
            d.value = 0

            i.value += 2
        
        self.data.primes = {0 : 2}
        sp.if (n.value <= 1):
            self.data.last_prime = 2
        sp.else:
            self.data.last_prime = i.value - 2
