let uporabi f a = f a

let ibarup x f = f x

let zacetnih n sez =
    let rec pomozna acc m s =
        if m <= 0 then Some acc
        else
            match s with
            | [] -> None
            | x::xs -> pomozna (x::acc) (m-1) xs
    in
    pomozna [] n sez

type 'a neprazen_sez = 
    | Konec of 'a 
    | Sestavljen of 'a * 'a neprazen_sez

let prvi sez = match sez with
    | Konec a -> a
    | Sestavljen (a, ostali_del) -> a

let rec zadnji sez = match sez with
    | Konec a -> a 
    | Sestavljen (a, s) -> zadnji s

let rec dolzina sez = match sez with
    | Konec a -> 1
    | Sestavljen (a, s) -> 1 + dolzina s

let rec pretvori_v_seznam sez = match sez with
    | Konec a -> [a]
    | Sestavljen (a, s) -> a :: (pretvori_v_seznam s)

let rec zlozi f a sez = match sez with
    | Konec b -> f a b
    | Sestavljen (b, preostanek) -> zlozi f (f a b) preostanek