(* 

   matvec.ml
   Operations with vectors and matrices. 

   Andrei de A. Formiga, 2013-03-17

*)

let matvec_mult mat vec = 
  let n = Array.length mat in 
  if Array.length vec != n then failwith "matvec_mult: incompatible dimensions"
  else
    let res = Array.create n 0.0 in 
    for i = 1 to n do
      res.(i) <- 0.0;
      for j = 1 to n do 
        res.(i) <- res.(i) +. mat.(i).(j) *. vec.(j)
      done
    done;
    res

(** Makes v1 <- v1 + v2 with v1 and v2 vectors *)
let vector_displace v1 v2 = 
  let n = Array.length v1 in 
  if Array.length v2 != n then failwith "vector_displace: incompatible dimensions"
  else
    for i = 1 to n do
      v1.(i) <- v1.(i) +. v2.(i)
    done

(** Cholesky decomposition. Given a symmetric positive definite matrix A, 
    return a lower-triangular matrix L such that A = LL' *)
let cholesky a = 
  a
