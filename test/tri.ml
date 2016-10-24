(*
*)
let rec pow x m = if m = 0 then 1.0 else x *. pow x (m-1) in
let rec fact n = if n < 2 then 1 else n * fact (n-1) in
let pi = 3.141592653589793238 in
let rec fneg x = 0.0 -. x in
let rec abs_float x = if x > 0.0 then x else fneg x in
let rec even n =
  if n = 0 then true
  else if n = 1 then false
  else even (n-2)
in

let rec sin x =
  if x < 0.0 then fneg (sin (fneg x)) else
  if x > pi /. 2.0 then sin (pi -. x) else
  let rec g n =
    let m = 2 * n + 1 in
    let f = pow x m /. float_of_int (fact m) in
    if even n then f else fneg f
  in g 0  +. g 1  +. g 2  +. g 3  +. g 4 +. g 5
in

let rec cos x =
  if x < 0.0 then cos (fneg x) else
  if x > pi /. 2.0 then fneg (cos (pi -. x)) else
  let rec g n =
    let m = 2 * n in
    let f = pow x m /. float_of_int (fact m) in
    if even n then f else fneg f
  in g 0  +. g 1  +. g 2  +. g 3  +. g 4 +. g 5
in

let rec atan x =
  if fless 1.0 x then pi /. 2. -. atan (1. /. x) else
  let rec f n =
    let rec g k = (2. *. float_of_int k) /. (2. *. float_of_int k +. 1.) in
    let rec h m = if m = 0 then 1. else (h (m-1)) *. g m in
    h n *. pow (pow x 2 /. (1. +. pow x 2)) n
  in
  (x /. (1. +. pow x 2)) *.
  (f  0 +. f  1 +. f  2 +. f  3 +. f  4 +.
   f  5 +. f  6 +. f  7 +. f  8 +. f  9 +.
   f 10 +. f 11 +. f 12 +. f 13 +. f 14 +.
   f 15
  )
in

print_float (sin 0.0 +. cos 0.0 +. atan 0.0)

