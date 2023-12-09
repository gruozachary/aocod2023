import itertools

class Solver:
    def __init__(self, inp):
        self.inp = inp

    def get_inp(self, part):
        res = self.inp
        if part == 2:
            res = [xs[::-1] for xs in res]
        return res

    def diffs(self, xs):
        cs = [xs]
        while not all([x==0 for x in cs[-1]]):
            ps = itertools.pairwise(cs[-1])
            cs.append([y-x for x, y in ps])
        return cs

    def calc(self, cs):
        l = 0
        for ps in cs[::-1]:
            l += ps[-1]
        return l

    def solve(self, part):
        s = 0
        for xs in self.get_inp(part):
            s += self.calc(self.diffs(xs))
        return s

ss = open("input.txt").read().split("\n")
xs = [[int(x) for x in xs.split()] for xs in ss]

sl = Solver(xs)
print(sl.solve(part=1))
print(sl.solve(part=2))
