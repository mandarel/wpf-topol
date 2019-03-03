(******************************************)
(* Recenzent:   Michał Makowski           *)
(* Autor:       Krzysztof Rolf            *)
(* Zadanie:     Sortowanie topologiczne   *)
(******************************************)

exception Cykliczne

(* funkcja pomocnicza tworzaca liste wierzcholkow *)
let znajdz_wierzcholki macierz = 
  let rec wierzcholki macierz a= 
    match macierz with 
    | [] -> List.rev a
    | (h, _)::t -> wierzcholki t (h::a)
    in wierzcholki macierz [];;

(* funkcje pomocnicze tworzace mape PMap z wierzcholkami *)
let dodaj_do_grafu l = 
  let rec pom graf l1 = 
    match l1 with 
    | [] -> graf 
    | h::t -> pom (PMap.add h [] graf) t
    in pom PMap.empty l;;

(* dodaje sasiadow do grafu z wierzcholkami*)
let dodaj_sasiadow_do_grafu g l = 
  let rec pom graf l1 = 
    match l1 with 
    | [] -> graf 
    | (a, l2)::t ->
    let m = try PMap.find a graf with Not_found -> [] in 
    pom (PMap.add a (m@l2) graf) t  
    in pom g l;;

(* funkcja pomocnicza tworzaca mape PMap z listy wierzcholkow i listy (wierzcholkow * list sasiadow) *)
let stworz_mape l1 l2= 
  let graf_z_wierzcholkami = dodaj_do_grafu l1 in 
  let graf_pelen = dodaj_sasiadow_do_grafu graf_z_wierzcholkami l2 in 
  graf_pelen;;

(* dfs (lista posortowana topologicznie, PMap elementow listy posortowanej topologicznie, PMap elementow drogi) | a' wierzcholek z ktorego zaczynamy | PMap elementow listy (wierzcholkow * list sasiadow) *)
let dfs (odw, odw2, dro) zrodlo mapa = 
  let rec sort (odwiedzone, odwiedzone2, droga)  = function 
  | [] -> (odwiedzone, odwiedzone2, droga)
  | h::t -> 
      if PMap.mem h droga 
        then 
         if PMap.mem h odwiedzone2 then (odwiedzone, odwiedzone2, droga) else raise Cykliczne
        else  
        let (odwiedzone', odwiedzone2', drogam) = List.fold_left (fun acc x -> sort acc [x]) (odwiedzone, odwiedzone2, (PMap.add h 0 droga)) ( try PMap.find h mapa with Not_found -> [] ) 
  in sort (h::odwiedzone', PMap.add h 0 odwiedzone2', drogam) t
  in sort (odw, odw2, dro) [zrodlo];;

(* sortowanie topologiczne korzystajace z dfs (wyszukiwania w glab) do ustalenia kolejnosci elementow *)
(* przeksztalca liste (wierzcholkow * list sasiadow) w liste wierzcholkow i tworzy mape w PMap *)
(* przechodzi po wszystkich elementach w l korzystajac z dfs *)
let topol l = 
  let w1 = znajdz_wierzcholki l in 
  let mapa = stworz_mape w1 l in 
  let (z, _, _) = (List.fold_left ( fun acc (x, _) -> dfs acc x mapa) ([],PMap.empty,PMap.empty) l) in 
  z;;

(* testy *)
(* Funkcje testujące - zle, test, isOk
 * 
 * Autor: Agnieszka Świetlik
 * Licence: Unlicensed
 * Original repo: https://github.com/aswietlik/wpf *)

let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end

let isOk input output =
	let rec where a = function
		| [] -> []
		| h::t -> if h = a then h::t else (where a t) in 
	let rec length used = function
		| [] -> List.length used
		| (h, l1)::t -> let newOne used a = 
					if (where a used) = [] then a::used else used
				in length (newOne (List.fold_left newOne used l1) h) t in
	let size = length [] input in
	let rec find acc wh = function
		| [] -> acc
		| h::t -> acc && ((where h wh) <> []) && (find acc wh t) in
	let rec pom acc = function
		| [] -> acc
		| (a, l)::t -> acc && find acc (where a output) l && pom acc t
	in (size = List.length output) && (pom true input);;

let genTest n =
	let res = ref [] in
	for i = 1 to (n-1) do
		if Random.int 10 < 6 then
		let l = ref [] and m = Random.int (n - i) in
		let used = Array.make (n+1) false in
		let unUsed _ =
			let getInt n = min n ((Random.int (max (n - i - 1) 1)) + i + 1) in
			let a = ref (getInt n) in
			while used.(!a) = true do
				if (!a) = n then a := (i + 1) else a := (!a) + 1;
			done; used.(!a) <- true; !a in
		for j = 0 to m do
			l := (unUsed j)::(!l)
		done;
		res := (i, !l)::(!res)
	done; (!res);;

  let lista n = 
    let rec pom n1 a =
      if n1 > 0 then pom (n1-1) (n1::a) else a
    in pom n [];;
  
  let gen n =
    let rec pom n1 a = 
    if n1> 0 then pom (n1-1) ((n1+1, (lista n1))::a) else a in pom n [];;
  
  
  let w1 = [("i", ["c"]);("e", ["m"]);("f", ["i";"e"]);("m", ["c";"a"])];;
  let w2 = [ ( ("a", 1), [ ("c", 2) ] ); ( ("e", 3), [ ("g", 4) ] ); ( ("f", 5), [ ("a", 1); ("e", 3) ] ); ( ("g", 4), [ ("c", 2); ( "a", 1) ] ) ];;
  let w3 = [ (1,[2]); (2,[3]); (3,[4]); (4,[5]); (5,[6]); (6,[7]); (7,[8]); (9,[10]); (11,[12]); (12,[1])];;
  let w4 = gen 500 ;; (* pamieciożerne dla 50 000*)
  let w5 = [("i", ["c"]);("e", ["m"]);("f", ["i";"e"]);("m", ["c";"a"]); ("w", [])];;
  let w6 = [ (1, [2]); (1, [3]) ];;
  test 1 (isOk w1 (topol w1));;
  test 2 (isOk w2 (topol w2));;
  test 3 (isOk w3 (topol w3));;
  test 4 (isOk w4 (topol w4));;
  test 5 (isOk w5 (topol w5));;
  test 6 (isOk w6 (topol w6));;  



  