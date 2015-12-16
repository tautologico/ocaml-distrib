(*

   matrix.ml
   Operations with vectors and matrices.

   Andrei de A. Formiga, 2013-03-17

*)

(***

  TODO

  - init
  - print/export in format for use with R
  - multiply by scalar

***)

(* A matrix of floats.
   The matrix is stored as a linear array of floats in row-major order *)
type t = {
    rows: int;
    cols: int;
    entries: float array;
  }

exception Incompatible_dimensions

let create ?(initval=0.0) ~rows ~cols =
  { rows=rows; cols=cols; entries=Array.make (rows*cols) initval }

(* a vector is just a column-matrix *)
let create_vector ?(initval=0.0) size =
  { rows=size; cols=1; entries=Array.make size initval }

let from_array ~rows ~cols a =
  if Array.length a != rows*cols then raise Incompatible_dimensions
  else { rows=rows; cols=cols; entries=a }

let init ~rows ~cols ~f =
  { rows=rows; cols=cols; entries=Array.init (rows*cols) (fun i -> f (i/cols) (i mod cols))}

let init_vector n f =
  { rows=n; cols=1; entries=Array.init n f }

let identity n =
  { rows=n; cols=n; entries=Array.init (n*n) (fun i -> if i mod (n+1) = 0 then 1.0 else 0.0)}

let rows m = m.rows

let columns m = m.cols

let vector_size v =
  if v.cols != 1 then raise Incompatible_dimensions
  else v.rows

let get m row col =
  m.entries.(row*m.cols+col)

let set m row col value =
  m.entries.(row*m.cols+col) <- value

let vector_from_array a =
  { rows=Array.length a; cols=1; entries=a }

let copy m1 =
  { rows=m1.rows; cols=m1.cols; entries=Array.copy m1.entries }

(* copy v as a column c in matrix m *)
let copy_vec_mat_col v m c =
  for i = 0 to m.rows-1 do
    set m i c (v.entries.(i))
  done

(* copy v as a row r in matrix m *)
let copy_vec_mat_row v m r =
  let ix = r * m.cols in
  for i = 0 to m.cols-1 do
      m.entries.(ix + i) <- v.entries.(i)
  done

(* operations *)
let mult m1 m2 =
  if m1.cols != m2.rows then raise Incompatible_dimensions
  else
    let res = create m1.rows m2.cols in
    let sum = ref 0.0 in
    for i = 0 to res.rows-1 do
      for j = 0 to res.cols-1 do
        sum := 0.0;
        for k = 0 to m1.cols-1 do
          sum := !sum +. (get m1 i k) *. (get m2 k j)
        done;
        set res i j !sum
      done
    done;
    res

let scmult m x =
  init ~rows:m.rows ~cols:m.cols ~f:(fun r c -> (get m r c) *. x)

let array_map2 ~f a1 a2 =
  Array.mapi (fun i x -> f x a2.(i)) a1

let add m1 m2 =
  if m1.rows != m2.rows || m1.cols != m2.cols then
    raise Incompatible_dimensions
  else
    { rows=m1.rows; cols=m1.cols;
      entries=array_map2 ~f:(+.) m1.entries m2.entries }

let sub m1 m2 =
  if m1.rows != m2.rows || m1.cols != m2.cols then
    raise Incompatible_dimensions
  else
    { rows=m1.rows; cols=m1.cols;
      entries=array_map2 ~f:(-.) m1.entries m2.entries }

(** Makes m1 <- m1 + m2 with m1 and m2 matrices *)
let addto m1 m2 =
  if m1.rows != m2.rows || m1.cols != m2.cols then
    raise Incompatible_dimensions
  else
    for i = 0 to m1.rows*m1.cols-1 do
      m1.entries.(i) <- m1.entries.(i) +. m2.entries.(i)
    done

let exists ~p m =
  let rec loop i =
    if i = m.rows*m.cols then false
    else if p m.entries.(i) then true
    else loop (i+1) in
  loop 0

let all ~p m =
  not (exists (fun x -> not (p x)) m)

(* TODO: change to check for relative difference instead of absolute *)
let eq_eps ?(eps=0.001) m1 m2 =
  let dif = sub m1 m2 in
  all ~p:(fun x -> abs_float x <= eps) dif


(** Cholesky decomposition. Given a symmetric positive definite matrix A,
    return an upper-triangular matrix U such that A = U'U.

    Does not verify if the matrix passed as argument is indeed symmetric
    positive definite.
*)
let cholesky a =
  if a.rows != a.cols then raise Incompatible_dimensions
  else
    let res = create a.rows a.cols in
    let sumsq_col_above c =
      let sum = ref 0.0 in
      for k = 0 to c-1 do
        sum := !sum +. ((get res k c) ** 2.0)
      done;
      !sum in
    let dotprodlines i j =
      let sum = ref 0.0 in
      for k = 0 to i-1 do
        sum := !sum +. (get res k i) *. (get res k j)
      done;
      !sum in
    set res 0 0 (sqrt (get a 0 0));
    for j = 1 to a.cols-1 do
      set res 0 j ((get a 0 j) /. (get res 0 0))
    done;
    for i = 1 to a.rows-1 do
      set res i i (sqrt ((get a i i) -. sumsq_col_above i));
      for j = i+1 to a.rows-1 do
        set res i j (((get a i j) -. (dotprodlines i j)) /. (get res i i))
      done
    done;
    res

(* TODO: adjustable format for printing entries *)
let print m =
  for i = 0 to m.rows-1 do
    print_string "|";
    for j = 0 to m.cols-1 do
      Printf.printf " %6.5f" (get m i j)
    done;
    print_endline " |"
  done

(* printing and export functions for R interop *)
let print_vector_r v =
  let printer i x =
    if i < v.rows-1 then ( print_float x; print_string ", " )
    else print_float x in
  print_string "c(";
  Array.iteri printer v.entries;
  print_endline ")"

(* print row r of matrix m as an R vector *)
let print_row_r m r =
  let ix = r * m.cols in
  print_string "c(";
  for i = 0 to m.cols-2 do
    Printf.printf "%f, " m.entries.(i+ix)
  done;
  Printf.printf "%f" m.entries.(ix+m.cols-1);
  print_endline ")"

(* convenience operators for local open *)
let ( + ) m1 m2 = add m1 m2
let ( - ) m1 m2 = sub m1 m2
let ( * ) m1 m2 = mult m1 m2
