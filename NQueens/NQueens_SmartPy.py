import smartpy as sp

class NQueens(sp.Contract):
    def __init__(self):
        self.init(m = {0 : 0}, used = {0 : 0})
        #"m" is the board //id is row, value is column
        #"used" are the spots where the current can/cannot be put in (1 is blocked, 0 is open)

    @sp.entry_point
    def n_queens(self, num):
        self.data.m = {}
        self.data.used = {}
        n = sp.local("n", num)          #number of queens to put on nxn board
        count = sp.local("count", 0)    #put on board so far
        k = sp.local("k", 0)            #go through the ones already on the board to compare to current one
        d = sp.local("d", 0)            #check diagonals
        v = sp.local("v", 0)            #add or sub to diagonals
        back = sp.local("back", False)  #check if its backtracking

        sp.if (n.value <= 0) | (n.value == 2) | (n.value == 3):
            self.data.m = {}
        sp.else:
            sp.while count.value < n.value:
                sp.if count.value == 0:
                    sp.if back.value == False:
                        self.data.m[count.value] = 0
                    sp.else:
                        self.data.m[count.value] += 1
                        
                        back.value = False

                    count.value += 1

                sp.else:
                    sp.while k.value < count.value:
                        v.value = self.data.m[k.value]
                        self.data.used[v.value] = 1
                        d.value = count.value - k.value

                        sp.if v.value + d.value < n.value:
                            self.data.used[v.value + d.value] = 1
                        
                        sp.if v.value - d.value >= 0:
                            self.data.used[v.value - d.value] = 1

                        k.value += 1
                    
                    sp.if back.value == True:
                        v.value = self.data.m[count.value]
                        k.value = 0
                        sp.while k.value < v.value + 1:
                            self.data.used[k.value] = 1
                            k.value += 1
                        back.value = False

                    k.value = 0

                    sp.while self.data.used.get(k.value, default_value = 0) == 1:
                        k.value += 1

                    sp.if k.value >= n.value:
                        back.value = True
                        count.value -= 1
                    sp.else:
                        self.data.m[count.value] = k.value
                        count.value += 1

                    k.value = 0
                    self.data.used = {}
