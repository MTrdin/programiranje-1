from functools import lru_cache

def najvec_jabolk(n, sadovnjak):
    vrstice = len(sadovnjak)
    stolpci = len(sadovnjak[0])
    def pomozna(i,j, koraki)
        if n <= 0:
            return 0
        if j < stolpci and i < vrstice:
            desno = pomozna(i, j+1, koraki-1 )
            naslednja = pomozna(i+1,0, koraki-1)
            return sadovnjak[i][j] + max(desno, naslednja)
        elif i < vrstice:
            #na koncu stolpca
            naslednja = pomozna(i+1,0, koraki-1)
            return sadovnjak[i][j] + naslednja
        elif j < stolpci:
            desno = pomozna(i, j+1, koraki-1 )
            return sadovnjak[i][j] + desno
        else:
            return sadovnjak[i][j]
        
    return pomozna(0,0,n)