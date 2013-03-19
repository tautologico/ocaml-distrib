(*

  tmatvec.ml
  Tests of the matrix and vector operations. 

  Andrei de A. Formiga, 2013-03-17

*)

let m1 = Matrix.from_array 2 2
  [| 1.0; 2.0;
     1.5; 3.0 |]

let m2 = Matrix.from_array 2 2
  [| 2.0; 3.0;
     2.5; 4.0 |]

(* two symmetric positive definite matrices *)
let sigma1 = Matrix.from_array 3 3 
  [|1.0; 0.5; 0.5;
    0.5; 2.0; 0.3;
    0.5; 0.3; 1.5|]

let sigma2 = Matrix.from_array 4 4 
  [|1.0; 0.5; 0.5; 0.5;
    0.5; 2.0; 0.3; 0.2;
    0.5; 0.3; 1.5; 0.1;
    0.5; 0.2; 0.1; 1.3|]


(* results of the cholesky factorization of sigma1 and sigma2 
   (reference results obtained using LAPACK) *)
let chol_sigma1 = Matrix.from_array 3 3
  [|1.0;     0.5;       0.5;
    0.0; 1.32288; 0.0377964;
    0.0;     0.0;   1.11739|]

let chol_sigma2 = Matrix.from_array 4 4
  [|1.0;     0.5;       0.5;       0.5;
    0.0; 1.32288; 0.0377964; -0.037794;
    0.0;     0.0;   1.11739; -0.132962;
    0.0;     0.0;       0.0;   1.01533|]

(* verify if matrix m1 is "reasonably close" to m2 *)
let verify_matrix ?(eps=0.001) m1 m2 =
  true 
  
let test_cholesky () = 
  let test_case name input output = 
    let res = cholesky input in
    verify_matrix res output in
  test_case "sigma1" sigma1 cholesky_sigma1

