(*

  test_matrix.ml
  Tests of the matrix and vector operations. 

  Andrei de A. Formiga, 2013-03-17

*)


(* utility function to create tests using fuzzy equality for matrices *)
let case_eps name ~code ~res = 
  Test.create_case name code (Matrix.eq_eps res)


(* Tests for simple operations *)
let m1 = Matrix.from_array 2 2
  [| 1.0; 2.0;
     1.5; 3.0 |]

let m2 = Matrix.from_array 2 2
  [| 2.0; 3.0;
     2.5; 4.0 |]

let m3 = Matrix.from_array ~rows:3 ~cols:2
  [|  2.5;   3.5; 
      4.2;   7.3; 
     1.23;  -9.18 |]

let m1_plus_m2 = Matrix.from_array 2 2
  [| 3.0; 5.0;
     4.0; 7.0 |]

let m2_minus_m1 = Matrix.from_array 2 2
  [| 1.0; 1.0;
     1.0; 1.0 |]

let m1_times_m2 = Matrix.from_array 2 2
  [|  7.0; 11.0; 
     10.5; 16.5 |]

let m2_times_m1 = Matrix.from_array 2 2
  [| 6.5; 13.0; 
     8.5; 17.0 |]

let m3_times_m2 = Matrix.from_array ~rows:3 ~cols:2
  [|  13.75; 21.5;
      26.65; 41.8;
     -20.49; -33.03 |]

(* two symmetric positive definite matrices *)
let sigma1 = Matrix.from_array 3 3 
  [| 1.0; 0.5; 0.5;
     0.5; 2.0; 0.3;
     0.5; 0.3; 1.5 |]

let sigma2 = Matrix.from_array 4 4 
  [| 1.0; 0.5; 0.5; 0.5;
     0.5; 2.0; 0.3; 0.2;
     0.5; 0.3; 1.5; 0.1;
     0.5; 0.2; 0.1; 1.3 |]


(* results of the cholesky factorization of sigma1 and sigma2 
   (reference results obtained using LAPACK) *)
let chol_sigma1 = Matrix.from_array 3 3
  [| 1.0;     0.5;       0.5;
     0.0; 1.32288; 0.0377964;
     0.0;     0.0;   1.11739 |]

let chol_sigma2 = Matrix.from_array 4 4
  [| 1.0;     0.5;       0.5;       0.5;
     0.0; 1.32288; 0.0377964; -0.037794;
     0.0;     0.0;   1.11739; -0.132962;
     0.0;     0.0;       0.0;   1.01533 |]


let basic_ops = Test.create_batch "Basic operations" 
    [
     case_eps "test_add" ~code:(fun () -> Matrix.add m1 m2) ~res:m1_plus_m2; 
     case_eps "test_sub" ~code:(fun () -> Matrix.sub m2 m1) ~res:m2_minus_m1;
     case_eps "test_mult1" ~code:(fun () -> Matrix.mult m1 m2) ~res:m1_times_m2;
     case_eps "test_mult2" ~code:(fun () -> Matrix.mult m2 m1) ~res:m2_times_m1;
     case_eps "test_mult3" ~code:(fun () -> Matrix.mult m3 m2) ~res:m3_times_m2
    ]

let errors = Test.create_batch "Wrong calls"
    [
     Test.create_case_exn "test_add2" 
       ~code:(fun () -> Matrix.add m1 sigma1)
       Matrix.Incompatible_dimensions
    ]

let chol_batch = Test.create_batch "Cholesky decomposition"
    [
     case_eps "sigma1" ~code:(fun () -> Matrix.cholesky sigma1) ~res:chol_sigma1;
     case_eps "sigma2" ~code:(fun () -> Matrix.cholesky sigma2) ~res:chol_sigma2
    ]

(* Run all test batches *)
let () = 
  Test.runner [errors];
  Test.runner [basic_ops; chol_batch]
