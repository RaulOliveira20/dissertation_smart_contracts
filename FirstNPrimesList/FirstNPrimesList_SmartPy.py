import smartpy as sp

class FirstNPrimesList(sp.Contract):
    def __init__(self):
        self.init(last_prime = sp.int(0), primes = sp.list())
    
    @sp.entry_point
    def n_primes(self, num):
        self.data.primes = [2]
        p = sp.local("p", 1)
        i = sp.local("i", 3)
        c = sp.local("c", 0)
        n = sp.local("n", num)
        
        sp.while p.value < n.value:
            
            sp.for x in self.data.primes:
                sp.if (i.value % x == 0):
                    c.value = 1

            sp.if (c.value == 0):
                self.data.primes.push(i.value)
                p.value += 1

            c.value = 0       

            i.value += 2
        
        self.data.primes = [2]
        sp.if (n.value <= 1):
            self.data.last_prime = 2
        sp.else:
            self.data.last_prime = i.value - 2
