try:
    from functools import lru_cache
except ImportError:
    from backports.functools_lru_cache import lru_cache

def zabica(mocvara):
    #zelimo ugotoviti v koliko skokih pride ven
    @lru_cache(maxsize=None)
    def pobeg(index,e):
        if index >= len(mocvara):
            return 0
        else:
            e += mocvara[index]
            return 1 + min([pobeg(index+d, e-d) for d in range(1, e+1)])
    return pobeg(0,0)