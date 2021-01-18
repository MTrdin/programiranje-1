type complex = { re : float; im : float }

let comlex_add a b = {re = a.re +. b.re; im = a.im +. b.im}

let complex_conjugate z = {re = z.re; im = -. z.im}

let rec list_apply_either pred f g sez = match sez with
    | [] -> []
    | x::xs -> if pred x then (f x)::list_apply_either xs else (g x)::list_apply_either xs

let int_list sez tocka =
    let rec pomozna acc_k s tocka_na_n = match s with
        | [] -> acc_k
        | koef::ostalo -> pomozna (koef * tocka_na_n + acc_k ) ostalo (tocka + tocka_na_n)
    in 
    pomozna 0 sez 1




type najemnik = string

type vrt = 
    | Obdelovan of lasnik
    | Oddan of lastnik * (vrt * vrt list)
    | Prost

let vrt_primer = Oddan("Kovalevska", 
    (Obdelovan "Galois", 
    Prost :: 
    [Obdelovan "Lagrange"]))

let obdelovalec_vrta = function
    | Oddan x -> None
    | Prost -> None
    | Obdelovan x -> Some x

let rec globina_oddajanja = function
    | Obdelovan _ -> 0
    | Prost -> 0
    | Oddan (_, (podvrt, podvrtovi)) -> 
        let globine = List.map globina_oddajanja podvrtovi in
        1 + List.fold_left max (globina_oddajanja podvrt) globine
    
let rec v_uprabi = function
    | Obdelovan _ -> true
    | Prost -> false
    | Oddan (_, (podvrt, vrtovi)) ->
        if v_uprabi podvrt then true
        else
            let sez_boolov = List.map v_uprabi vrtovi in
            List.exists v_uprabi sez_boolov

let rec vsi_najemniki = function
    | Prost -> []
    | Obdelovan oseba -> [oseba]
    | Oddan (oddajalec, (podvrt, podvrtovi)) -> 
        let vsi_podnajemniki =
            List.fold_left (fun acc vrt -> vsi_najemniki vrt @ acc) [] podvrtovi
        in
        oddajalec :: vsi_najemniki podvrt @ vsi_podnajemniki

let rec vsi_obdelovalci = function
    | Prost -> []
    | Obdelovan oseba -> [oseba]
    | Oddan (oddajalec, (podvrt, podrvtovi)) -> 
        let vsi_podobdelovalci =
            List.fold_left (fun acc vrt -> vsi_obdelovalci vrt @ acc) [] podvrtovi
        in
        vsi_obdelovalci podvrt @ vsi_podobdelovalci