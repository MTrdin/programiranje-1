let rec izpisi_vsa_stevila sez = match sez with
    | [] -> ()
    | x::xs -> print_int x; izpisi_vsa_stevila xs

let rec map2_opt f sez1 sez2 =
    let rec pomozna acc s1 s2 = match s1, s2 with
    | [], _::_ -> None
    | _::_, [] -> None
    | [],[] -> Some List.rev acc
    | x::xs, y::ys -> pomozna ((f x y)::acc) xs ys
    in
    pomozna [] sez1 sez2

type filter_tree = 
    | Vozlisce of (int * filter_tree * filter_tree)
    | List of int list

let primer = Vozlisce(
    10,
    Vozlisce(5, List [1], List []),
    Vozlisce(15, List [], List [19,20])
)

let rec vstavi st drevo = match drevo with
    | List sez -> List st::sez
    | Vozlisce (a, levo, desno) -> 
        if st <= a then Vozlisce (a, vstavi st levo, desno)
        else Vozlisce(a, levo, vstavi st desno)

let rec vstavi_sez sez drevo = match sez with
    | [] -> drevo
    | x::xs -> vstavi_sez xs (vstavi x drevo)

let rec pravilno_razvrsceno drevo = match drevo with
    | List sez -> true
    | Vozlisce(a,levo,desno) ->(
        pravilno_razvrsceno levo && pravilno_razvrsceno desno
    )