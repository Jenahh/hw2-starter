let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (a, b) = (b, a)

let rev_triple (a, b, c) = (c, b, a)

let is_odd x = x mod 2 <> 0

let is_older (y1, m1, d1) (y2, m2, d2) =
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else d1 < d2

let to_us_format (y, m, d) = (m, d, y)
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p =
  if p = 0 then 1 else x * pow x (p - 1)

let rec fac n =
  if n = 1 then 1 else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth (idx, lst) =
  match (idx, lst) with
  | 0, x :: _ -> x
  | n, _ :: xs when n > 0 -> get_nth (n - 1, xs)
  | _, [] -> failwith "get_nth: index out of bounds"
  | _ -> failwith "get_nth: negative index"

let larger lst1 lst2 =
  let l1 = List.length lst1 and l2 = List.length lst2 in
  if l1 > l2 then lst1 else if l2 > l1 then lst2 else []

let sum lst1 lst2 =
  let rec sum_list acc = function
    | [] -> acc
    | x :: xs -> sum_list (acc + x) xs
  in
  sum_list 0 lst1 + sum_list 0 lst2