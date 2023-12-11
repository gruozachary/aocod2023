from collections import Counter
from functools import cmp_to_key

F = open("input.txt").read()
H = [L.split() for L in F.split("\n")]
H = [(h, int(b)) for [h, b] in H]

class Solver:
    def __init__(self, p2):
        self.cpd = ['A', 'K', 'Q', 'J' ,'T', '9', '8', '7', '6', '5', '4', '3', '2'] 
        if p2: self.cpd.append(self.cpd.pop(3))
        self.p2 = p2
    
    def rateHand(self, h):
        hs = Counter(h)
        if self.p2:
            for (k, _) in hs.most_common():
                if k != "J":
                    hs[k] += hs["J"]
                    if hs.get("J"):
                        hs.pop("J")
        res = 6
        match sorted(hs.values()):
            case [5]: res = 0
            case [1, 4]: res = 1
            case [2, 3]: res = 2
            case [1, 1, 3]: res = 3
            case [1, 2, 2]: res = 4
            case [1, 1, 1, 2]: res = 5
        return res
    
    def cmp(self):
        def f(c1, c2):
            (h1, _) = c1
            (h2, _) = c2
            r1, r2 = self.rateHand(h1), self.rateHand(h2)
            if r1 < r2: return 1 
            if r1 > r2: return -1
            for (c1, c2) in zip(h1, h2):
                i1, i2 = self.cpd.index(c1), self.cpd.index(c2)
                if i1 != i2:
                    return i2 - i1
            return 0
        return f
        
    def solve(self, H):
        H1 = sorted(H, key=cmp_to_key(self.cmp()))
        return sum([(i+1)*b for (i, (_, b)) in enumerate(H1)])


p1 = Solver(False)
p2 = Solver(True)
print(p1.solve(H))
print(p2.solve(H))
