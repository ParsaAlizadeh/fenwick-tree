from random import *

N = (1 << 19)
M = 6 * N
C = 1000000000
QWEIGHTS = [2, 1, 1]

print(N)
for _ in range(M):
    typ, = choices([0, 1, 2], QWEIGHTS)
    if typ == 0:
        ind = randint(1, N)
        val = randint(-C, C)
        print(typ, ind, val)
    elif typ == 1:
        r = randint(1, N)
        print(typ, r)
    elif typ == 2:
        l = randint(1, N)
        r = randint(1, N)
        if l > r:
            l, r = r, l
        print(typ, l, r)
print(-1)