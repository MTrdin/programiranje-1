
def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x not in rezultati:
            rezultati[x] = f(x)
        return rezultati[x]
    return mem_f

@memoiziraj
def stevilo_postavitev(sirina_balkona, st_korit,l):
    if st_korit <= 0:
        return 1
    elif l > sirina_balkona:
        return 0
    
    na_prvo = stevilo_postavitev(sirina_balkona-l-1, st_korit-1,l)
    brez_prve = stevilo_postavitev(sirina_balkona-1, st_korit, l)

    return na_prvo + brez_prve

@memoiziraj
def st_postavitev_sez(n, sez):
    if n < sum(sez):
        return 0
    elif sez == []:
        return 1
    elif n < 1:
        return 1

    na_prvo = st_postavitev_sez(n-sez[0]-1, sez[1:])
    brez_prve = st_postavitev_sez(n-1, sez)
    return na_prvo + brez_prve

#iz resitev
def blocko(n, blocks):
    @lru_cache(maxsize=None)
    def blocker(n, i):
        if i >= len(blocks) and n >= -1:
            return 1
        elif n <= 0:
            return 0
        else:
            return blocker(n-1, i) + blocker(n-blocks[i]-1, i+1)
    return blocker(n, 0)
