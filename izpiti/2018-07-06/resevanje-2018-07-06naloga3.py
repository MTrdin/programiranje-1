
#from functools import lru_cache


def memoiziraj(f):
    rezultati = {}
    def mem_f(x):
        if x not in rezultati:
            rezultati[x] = f(x)
        return rezultati[x]
    return mem_f


def simetricen(niz):
    return niz == niz[::-1]

#@lru_cache(maxsize=None)
@memoiziraj
def stevilo_delov(niz):
    if niz == "":
        return 0
    if simetricen(niz):
        return 1
    
    moznosti = [stevilo_delov(niz[i:]) + stevilo_delov(niz[:i]) for i in range(1, len(niz))]

    return min(moznosti)

#@lru_cache(maxsize=None)
@memoiziraj
def razdeli(niz):
    if niz == "":
        return (0,[niz])
    if simetricen(niz):
        return (1,[niz])

    najbojsa_moznost = None
    for i in range(1, len(niz)):
        st_l, sez_l = razdeli(niz[:i])
        st_d, sez_d = razdeli(niz[i:])
        k, sez = st_l + st_d, sez_l + sez_d

        if najbojsa_moznost is None:
            najbojsa_moznost = (k,sez)
        if k < najbojsa_moznost[0]:
            najbojsa_moznost = (k,sez)

    return najbojsa_moznost[1]

#print(razdeli("00101011"))

def vsotno_simetricen(niz):
    d = len(niz) // 2
    leva_vsota = 0
    desna_vsota = 0
    for i, znak in enumerate(niz):
        if i < d:
            leva_vsota += int(znak)
        else:
            desna_vsota += int(znak)
    return desna_vsota % 2 == leva_vsota % 2

#print(vsotno_simetricen("01001000"))
#print(vsotno_simetricen("1011"))

@memoiziraj
def stevilo_delov_e(objekt,je_simetricen):
    if objekt == []:
        return 0
    if je_simetricen(objekt):
        return 1
    
    moznosti = [stevilo_delov_e(objekt[i:], je_simetricen) + stevilo_delov_e(objekt[:i], je_simetricen) for i in range(1, len(objekt))]

    return min(moznosti)