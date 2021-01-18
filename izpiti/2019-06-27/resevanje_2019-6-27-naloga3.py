#try:
#    from functools import lru_cache
#except ImportError:
#    from backports.functools_lru_cache import lru_cache

from functools import lru_cache
# ta ni dobra
#@lru_cache(maxsize=None)
def st_nacinov_natovarjanja(nosilnost, zabojniki):
    if nosilnost <= 0:
        return 0
    elif zabojniki == []:
        return 1
    else:
        return sum(st_nacinov_natovarjanja(nosilnost-k, zabojniki) for k in zabojniki)

def nacini_natovarjanja(nosilnost, zabojniki):
    @lru_cache(maxsize=None)
    def nacini_aux(k, nos):
        if nos == 0:
            return 1
        if k >= len(zabojniki):
            return 0
        
        n = 0
        teza_zabojnika = zabojniki[k]
        if teza_zabojnika <= nos:
            n += nacini_aux(k, nos - teza_zabojnika)
        n += nacini_aux(k+1, nos)
        return n

    return nacini_aux(0, nosilnost)


#primer = nacini_natovarjanja(5, [1,3,4,7])
#print(primer)