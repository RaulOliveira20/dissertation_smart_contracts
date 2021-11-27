import smartpy as sp

#program gets a magic square of NxN with superposition (Euler's method)
#N = 6 doesn't work with this method
class MagicSquareSP(sp.Contract):
    def __init__(self):
        self.init(a = {0 : 0}, b = {0 : 0}, used = {0 : 0}, use = {0 : 0}, m = {0 : 0})
        #"a" is the first square
        #"b" is the second square
        #"used" is the numbers left to use (or in use)
        #"use" is same as "used" (to check both squares at once)
        #"used" is for "a", "use" is for "b"
        #"m" is 1st used to check the pairs of numbers being used 
        #and at the end is the final square which should be magic

    @sp.entry_point
    def magic_square(self, num):
        self.data.a = {}
        self.data.b = {}
        self.data.used = {}
        self.data.use = {}
        self.data.m = {}
        n = sp.local("n", num)
        i = sp.local("i", 0)
        j = sp.local("j", 0)
        s = sp.local("s", 0)
        k = sp.local("k", 0)
        t = sp.local("t", 0)
        p = sp.local("p", 0)
        back = sp.local("back", False)
        b_o = sp.local("b_o", 0)
        b = sp.local("b", False)
        #b_o is used to check if either "used" or "use" overflowed
        #0 = no overflow or "use" overflowed, 1 = "used" overflowed
        #2 = both "used" and "use" overflowed

        t.value = abs(n.value - 1) / abs(2)
        k.value = abs(n.value - 1)
        
        #fill main diagonal of "a" and 2nd diagonal of "b"
        sp.while (i.value < n.value):
            sp.if (n.value % 2 == 0):
                t.value = i.value

            s.value = i.value * n.value + j.value
            p.value = i.value * n.value + k.value
            self.data.a[s.value] = t.value
            self.data.b[p.value] = t.value
            i.value = i.value + 1
            j.value = j.value + 1
            k.value = abs(k.value - 1)

        i.value = abs(n.value - 1)
        j.value = 0
        p.value = 0
        k.value = 0
        t.value = abs(n.value - 1) / abs(2)

        sp.if (n.value % 2 == 1):
            self.data.used[t.value] = 1
            self.data.m[t.value * n.value + t.value] = 1

        #fill 2nd diagonal of "a" and main diagonal of "b"
        sp.while (j.value < n.value):
            sp.if (i.value == j.value) & (n.value != 1):
                i.value = abs(i.value - 1)
                j.value = j.value + 1
                p.value = p.value + 1

            s.value = i.value * n.value + j.value
            
            sp.if (n.value % 2 == 1):
                sp.while (self.data.used.get(k.value, default_value = 0) == 1):
                    k.value = k.value + 1
            sp.else:
                sp.while (self.data.used.get(k.value, default_value = 0) == 1) | (k.value == i.value) | (k.value == j.value):
                    k.value = k.value + 1

            self.data.a[s.value] = k.value
            self.data.used[k.value] = 1

            s.value = k.value * n.value + self.data.b[s.value]
            self.data.m[s.value] = 1

            s.value = p.value * n.value + j.value
            self.data.b[s.value] = k.value

            s.value = self.data.a[s.value] * n.value + k.value
            self.data.m[s.value] = 1

            i.value = abs(i.value - 1)
            j.value = j.value + 1
            p.value = p.value + 1
            k.value = 0

        self.data.used = {}
        i.value = 0
        j.value = 0
        k.value = 0
        
        #filling the rest of "a" and "b" with backtracking
        sp.while (i.value < abs(n.value - 1)) | (j.value < abs(n.value - 1)):
            t.value = i.value + j.value
            #checks if current position is one of the diagonals
            sp.if (i.value == j.value) | (t.value == abs(n.value - 1)):
                sp.if (back.value == False):
                    sp.if (j.value < abs(n.value - 1)):
                        j.value = j.value + 1
                    sp.else:
                        i.value = i.value + 1
                        j.value = 0
                sp.else:
                    sp.if (j.value > 0):
                        j.value = abs(j.value - 1)
                    sp.else:
                        i.value = abs(i.value - 1)
                        j.value = abs(n.value - 1)
                    
            sp.else:
                k.value = 0

                #go through current line/column (of "a" and "b")
                #to mark the values that can't be chosen in 
                #"a" on "used" and in "b" on "use"
                sp.while (k.value < n.value):
                    sp.if (k.value != j.value):
                        s.value = i.value * n.value + k.value

                        p.value = self.data.a.get(s.value, default_value = n.value)
                        sp.if (p.value != n.value):
                            self.data.used[p.value] = 1

                        p.value = self.data.b.get(s.value, default_value = n.value)
                        sp.if (p.value != n.value):
                            self.data.use[p.value] = 1

                    sp.if (k.value != i.value):
                        s.value = k.value * n.value + j.value
                        
                        p.value = self.data.a.get(s.value, default_value = n.value)
                        sp.if (p.value != n.value):
                            self.data.used[p.value] = 1                  

                        p.value = self.data.b.get(s.value, default_value = n.value)
                        sp.if (p.value != n.value):
                            self.data.use[p.value] = 1
                        
                    k.value = k.value + 1
                
                #restrict the position a(1.0) for a faster filling
                sp.if (i.value == 1) & (j.value == 0):
                    k.value = 0
                    
                    sp.if (n.value % 2 == 0):
                        t.value = n.value / 2
                    sp.else:
                        t.value = abs(n.value - 1)

                    sp.while (k.value < t.value):
                        self.data.used[k.value] = 1
                        k.value = k.value + 1

                #restrict the position b(1.2) for a faster filling
                sp.if (i.value == 1) & (j.value == 2):
                    k.value = 0
                    
                    t.value = abs(n.value / 2 - 1)

                    sp.while (k.value < t.value):
                        self.data.use[k.value] = 1
                        k.value = k.value + 1

                #restrict the position b(1.3) for a faster filling
                sp.if (i.value == 1) & (j.value == 3):
                    k.value = 0
                    t.value = 3

                    sp.while (k.value < t.value):
                        self.data.use[k.value] = 1
                        k.value = k.value + 1

                sp.if (back.value == True):
                    s.value = i.value * n.value + j.value
                    k.value = self.data.a[s.value]
                    t.value = self.data.b[s.value]
                    self.data.a[s.value] = n.value
                    self.data.b[s.value] = n.value

                    s.value = k.value * n.value + t.value
                    self.data.m[s.value] = 0
                    
                    sp.if (b_o.value == 2):
                        k.value = k.value + 1
                        t.value = t.value + 1
                    sp.else:
                        sp.if (b_o.value == 1):
                            k.value = k.value + 1
                            t.value = 0
                        sp.else:
                            t.value = t.value + 1
                    
                sp.else:
                    k.value = 0
                    t.value = 0
                    b_o.value = 0

                #find open value to put in "a"
                sp.while (self.data.used.get(k.value, default_value = 0) == 1):
                    k.value = k.value + 1

                #find open value to put in "b"
                sp.while (self.data.use.get(t.value, default_value = 0) == 1):
                    t.value = t.value + 1

                ########################################
                #Possible scenarios from this point on:
                #1. back = false, no overflow, see if pair exists, go thru all pair opts. If all exist, back with b_o = 1
                #2. back = false, k overflow, backtrack with b_o = 1
                #3. back = false, t overflow, backtrack with b_o = 0
                #4. back = false, both k and t overflow, backtrack with b_o = 2
                #5. back = true, no overflow, see if pair exists, go thru all pair opts. If all exist, back with b_o = 1
                #6. back = true, k overflow, backtrack with b_o = 0
                #7. back = true, t overflow, t = 0, k = k + 1, go thru all pair opts. If all exist, back with b_o = 1
                #8. back = true, both k and t overflow, back with b_o = 0
                ########################################

                #this covers scenarios 1, 5 and 7
                sp.if ((k.value < n.value) & (t.value < n.value)) | ((back == True) & (k.value < n.value)):
                    sp.if (t.value >= n.value):
                        t.value = 0
                        k.value = k.value + 1

                    #go through all possible pairs, if all exist backtrack with b_o = 1
                    sp.while (k.value < n.value) & (b.value == False):
                        sp.while (self.data.used.get(k.value, default_value = 0) == 1):
                            k.value = k.value + 1

                        sp.while (self.data.use.get(t.value, default_value = 0) == 1):
                            t.value = t.value + 1

                        sp.if (t.value >= n.value) | (k.value >= n.value):
                            k.value = k.value + 1
                            t.value = 0
                        sp.else:
                            s.value = k.value * n.value + t.value
                            sp.if (self.data.m.get(s.value, default_value = 0) != 1):
                                self.data.m[s.value] = 1
                                s.value = i.value * n.value + j.value
                                self.data.a[s.value] = k.value
                                self.data.b[s.value] = t.value

                                b.value = True
                            sp.else:
                                t.value = t.value + 1
                                
                                sp.if (t.value >= n.value):
                                    k.value = k.value + 1
                                    t.value = 0

                    #if b is true, it means it found an open pair, if not, all were taken
                    sp.if (b.value == True):
                        sp.if (j.value < abs(n.value - 1)):
                            j.value = j.value + 1
                        sp.else:
                            i.value = i.value + 1
                            j.value = 0

                        b.value = False
                        back.value = False
                    sp.else:
                        sp.if (j.value > 0):
                            j.value = abs(j.value - 1)
                        sp.else:
                            i.value = abs(i.value - 1)
                            j.value = abs(n.value - 1)

                        back.value = True
                        b_o.value = 1

                sp.else:
                    #this covers scenarios 2, 3 and 4
                    sp.if (back.value == False):
                        sp.if (k.value >= n.value) & (t.value >= n.value):
                            b_o.value = 2
                        sp.else:
                            sp.if (k.value >= n.value):
                                b_o.value = 1
                            sp.else:
                                b_o.value = 0
                    #this covers scenarios 6 and 8
                    sp.else:
                        b_o.value = 0

                    sp.if (j.value > 0):
                        j.value = abs(j.value - 1)
                    sp.else:
                        i.value = abs(i.value - 1)
                        j.value = abs(n.value - 1)

                    back.value = True

                ####################################


                ########################################

                """
                sp.if (k.value >= n.value) | (t.value >= n.value):
                    sp.if (back.value == False) | (k.value >= n.value):
                        sp.if (j.value > 0):
                            j.value = abs(j.value - 1)
                        sp.else:
                            i.value = abs(i.value - 1)
                            j.value = abs(n.value - 1)
                    
                    sp.if (back.value == False):
                        sp.if (k.value >= n.value) & (t.value >= n.value):
                            b_o.value = 2
                        sp.else:
                            sp.if (k.value >= n.value):
                                b_o.value = 1
                            sp.else:
                                b_o.value = 0

                        back.value = True
                    sp.else:
                        b.value = False

                        sp.if (k.value >= n.value):
                            b_o.value = 0
                        sp.else:
                            k.value = k.value + 1
                            t.value = 0

                            sp.while (k.value < n.value) & (b.value == False):
                                sp.while (self.data.used.get(k.value, default_value = 0) == 1):
                                    k.value = k.value + 1

                                sp.while (self.data.use.get(t.value, default_value = 0) == 1):
                                    t.value = t.value + 1

                                sp.if (t.value >= n.value) | (k.value >= n.value):
                                    k.value = k.value + 1
                                    t.value = 0
                                sp.else:
                                    s.value = k.value * n.value + t.value
                                    sp.if (self.data.m.get(s.value, default_value = 0) != 1):
                                        self.data.m[s.value] = 1
                                        s.value = i.value * n.value + j.value
                                        self.data.a[s.value] = k.value
                                        self.data.b[s.value] = t.value

                                        b.value = True
                                    sp.else:
                                        t.value = t.value + 1
                                        
                                        sp.if (t.value >= n.value):
                                            k.value = k.value + 1
                                            t.value = 0

                            sp.if (b.value == True):
                                sp.if (j.value < abs(n.value - 1)):
                                    j.value = j.value + 1
                                sp.else:
                                    i.value = i.value + 1
                                    j.value = 0

                                b.value = False
                                back.value = False
                            sp.else:
                                sp.if (j.value > 0):
                                    j.value = abs(j.value - 1)
                                sp.else:
                                    i.value = abs(i.value - 1)
                                    j.value = abs(n.value - 1)

                                back.value = True
                                b_o.value = 1


                sp.else:
                    back.value = False
                    b.value = False
                    s.value = k.value * n.value + t.value

                    sp.if (self.data.m.get(s.value, default_value = 0) != 1):
                        self.data.m[s.value] = 1
                        s.value = i.value * n.value + j.value
                        self.data.a[s.value] = k.value
                        self.data.b[s.value] = t.value

                        sp.if (j.value < abs(n.value - 1)):
                            j.value = j.value + 1
                        sp.else:
                            i.value = i.value + 1
                            j.value = 0
                    sp.else:
                        sp.if (b_o.value == 0):
                            t.value = t.value + 1
                        sp.else:
                            k.value = k.value + 1
                            t.value = 0

                        sp.while (k.value < n.value) & (b.value == False):
                            sp.while (self.data.used.get(k.value, default_value = 0) == 1):
                                k.value = k.value + 1

                            sp.while (self.data.use.get(t.value, default_value = 0) == 1):
                                t.value = t.value + 1

                            sp.if (t.value >= n.value) | (k.value >= n.value):    
                                k.value = k.value + 1
                                t.value = 0
                            sp.else:
                                s.value = k.value * n.value + t.value
                                sp.if (self.data.m.get(s.value, default_value = 0) != 1):
                                    self.data.m[s.value] = 1
                                    s.value = i.value * n.value + j.value
                                    self.data.a[s.value] = k.value
                                    self.data.b[s.value] = t.value

                                    b.value = True
                                sp.else:
                                    t.value = t.value + 1
                                    
                                    sp.if (t.value >= n.value):
                                        k.value = k.value + 1
                                        t.value = 0
                        
                        sp.if (b.value == True):
                            sp.if (j.value < abs(n.value - 1)):
                                j.value = j.value + 1
                            sp.else:
                                i.value = i.value + 1
                                j.value = 0
                            b.value = False
                            back.value = False
                        sp.else:
                            sp.if (j.value > 0):
                                j.value = abs(j.value - 1)
                            sp.else:
                                i.value = abs(i.value - 1)
                                j.value = abs(n.value - 1)
                            back.value = True
                            b_o.value = 1
                """

                k.value = 0
                self.data.used = {}
                self.data.use = {}


            #sp.if (self.data.b.get(7, default_value = 0) == 4):
                #sp.if (self.data.a.get(5, default_value = 0) == 4):
                    #sp.if (self.data.a.get(11, default_value = 0) == 4):
                        #sp.if (self.data.b.get(11, default_value = 0) == 4):
            #sp.if (i.value == 5) & (j.value == 2):
                #i.value = n.value
                #j.value = n.value
            
            #sp.if (self.data.a.get(13, default_value = 0) == 4):

            #sp.if (self.data.a.get(27, default_value = 0) == 2):
            
            #self.data.i = back_over.value
            #sp.if (self.data.a.get(7, default_value = 0) == 6):
                #sp.if (self.data.a.get(2, default_value = 0) == 4):
            """
            sp.if (i.value == 2) & (j.value == 0):
                i.value = n.value
                j.value = n.value
            """
    


        """
        i.value = abs(n.value - 1)
        j.value = 0
        k.value = 0 #i coord for "b"
        t.value = 0 #j coord for "b"
        
        
        #rotating "a" and putting on "b"
        sp.while (j.value <= abs(n.value - 1)):
            s.value = i.value * n.value + j.value
            s.value = self.data.a[s.value]
            p.value = k.value * n.value + t.value
            self.data.b[p.value] = s.value

            sp.if (i.value > 0):
                i.value = abs(i.value - 1)
            sp.else:
                i.value = i.value + abs(n.value - 1)
                j.value = j.value + 1

            sp.if (t.value < abs(n.value - 1)):
                t.value = t.value + 1
            sp.else:
                k.value = k.value + 1
                t.value = 0
        """
        
        self.data.m = {}
        i.value = 0
        j.value = 0
        #multiplying "a" by N, then adding "b" and one
        sp.while (i.value < n.value):
            s.value = i.value * n.value + j.value
            t.value = self.data.a[s.value]
            k.value = self.data.b[s.value]
            p.value = t.value * n.value + k.value + 1
            self.data.m[s.value] = p.value

            sp.if (j.value < abs(n.value - 1)):
                j.value = j.value + 1
            sp.else:
                i.value = i.value + 1
                j.value = 0
        
        
        self.data.a = {}
        self.data.b = {}
        self.data.used = {}
        self.data.use = {}
    

@sp.add_test(name = "Testing getting an NxN magic square")
def test():
    scenario = sp.test_scenario()

    p = MagicSquareSP()
    scenario += p
    
    scenario += p.magic_square(7)