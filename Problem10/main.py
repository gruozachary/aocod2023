dmap = {
    "|": { "N", "S" },
    "-": { "E", "W" },
    "L": { "N", "E" },
    "J": { "N", "W" },
    "7": { "S", "W" },
    "F": { "S", "E" },
    ".": {},
    "S": { "N", "E", "S", "W" },
}

xs = open("input.txt").read().split("\n")

# Find start

start = None
for i, x in enumerate(xs):
    for j, y in enumerate(x):
        if y == "S":
            start = (i, j)

os = "NESWSWNE"
def opposite(d):
    return os[os.index(d)+4]

def get_neighbour(ix, d):
    i, j = ix
    ns = [ (i-1, j), (i, j+1), (i+1, j), (i, j-1) ]
    if d != None: ns[os.index(opposite(d))] = (-1, -1)
    for n, (k, l) in enumerate(ns):
        d = os[n]
        p = xs[i][j]
        if 0 <= k < len(xs) and 0 <= l < len(xs[k]) and\
                d in dmap[p] and opposite(d) in dmap[xs[k][l]]:
            return os[n], (k, l)
    return (-1, -1)


d, c = get_neighbour(start, None)
i = 1
while c != start:
    i += 1
    d, c = get_neighbour(c, d)
print(i // 2)
