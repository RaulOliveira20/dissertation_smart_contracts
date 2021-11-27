import smartpy as sp

class FirstNPrimes_V3(sp.Contract):
    def __init__(self):
        self.init(last_prime = sp.int(0))
    
    @sp.entry_point
    def n_primes(self, num):
        p = sp.local("p", 1)
        i = sp.local("i", 3)
        x = sp.local("x", 3)
        n = sp.local("n", num)
        
        sp.while p.value < n.value:
            
            sp.while (i.value % x.value != 0) & (x.value * x.value <= i.value):
                x.value += 2
            
            sp.if (i.value % x.value != 0) | (i.value == 3):
                p.value += 1
            
            x.value = 3
            
            i.value += 2
        
        sp.if (n.value <= 1):
            self.data.last_prime = 2
        sp.else:
            self.data.last_prime = i.value - 2
    
@sp.add_test(name = "Testing getting the first N prime numbers")
def test():
    scenario = sp.test_scenario()

    p = FirstNPrimes_V3()
    scenario += p
    
    scenario += p.n_primes(3)