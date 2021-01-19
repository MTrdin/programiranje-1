
def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x not in rezultati:
            rezultati[x] = f(x)
        return rezultati[x]
    return mem_f

@memoiziraj
def koliko_zaporedij(k, n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    
    takoj_naslednji_za_k = koliko_zaporedij(k, n-k)
    vec_kot_k = koliko_zaporedij(k, n-k-1)
    return takoj_naslednji_za_k + vec_kot_k

#print(koliko_zaporedij(3, 4))

from functools import lru_cache


def f(k, n):
    @lru_cache(maxsize=None)
    def count(a, k, n):
        if n == 1:
            return 1
        options = [count(x, k, n-1) for x in range(a-k, a+k+1) if x >= 0]
        return sum(options)
    return count(0, k, n)

