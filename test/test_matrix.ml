(*

  test_matrix.ml
  Tests of the matrix and vector operations. 

  Andrei de A. Formiga, 2013-03-17

*)


(* utility function to create tests using fuzzy equality for matrices *)
let case_eps name ~code ~res = 
  Test.create_case name code (Matrix.eq_eps res)


(* Tests for creation *)
let id3 = Matrix.from_array 3 3 
  [| 1.0; 0.0; 0.0; 
     0.0; 1.0; 0.0; 
     0.0; 0.0; 1.0 |]

let id4 = Matrix.from_array 4 4 
  [| 1.0; 0.0; 0.0; 0.0; 
     0.0; 1.0; 0.0; 0.0;
     0.0; 0.0; 1.0; 0.0;
     0.0; 0.0; 0.0; 1.0  |]

let init1 = Matrix.from_array 3 3 
  [|   1.0;   2.0;   3.0;
     101.0; 102.0; 103.0;
     201.0; 202.0; 203.0  |]


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

let v1 = Matrix.vector_from_array [| 1.2; 4.2 |]

let v2 = Matrix.vector_from_array [| 5.6; 11.2 |]

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

let m1_times_v1 = Matrix.vector_from_array [| 9.6; 14.4 |]

let m3_times_v2 = Matrix.vector_from_array [| 53.2; 105.28; -95.928 |]

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

(* copy results *)
let sigma1_cpcol_m3_times_v2 = Matrix.from_array 3 3 
  [|    53.2; 0.5; 0.5;
      105.28; 2.0; 0.3;
     -95.928; 0.3; 1.5   |]

let sigma1_cprow_m3_times_v2 = Matrix.from_array 3 3 
  [|  1.0;    0.5;     0.5;
     53.2; 105.28; -95.928;
      0.5;    0.3;     1.5  |]

(* results of multiplication by scalar *)
let sigma1x2 = Matrix.from_array 3 3 
  [| 2.0; 1.0; 1.0;
     1.0; 4.0; 0.6;
     1.0; 0.6; 3.0 |]


(* test batches *)
let creation = Test.create_batch "Creation"
    [
     case_eps "id3" ~code:(fun () -> Matrix.identity 3) ~res:id3;
     case_eps "id4" ~code:(fun () -> Matrix.identity 4) ~res:id4;
     case_eps "init1" 
       ~code:(fun () -> Matrix.init 3 3 
                        (fun r c -> 100.0 *. (float r) +. (float c) +. 1.0)) 
       ~res:init1
    ]

let basic_ops = Test.create_batch "Basic operations" 
    [
     case_eps "add" ~code:(fun () -> Matrix.add m1 m2) ~res:m1_plus_m2; 
     case_eps "sub" ~code:(fun () -> Matrix.sub m2 m1) ~res:m2_minus_m1;
     case_eps "mult1" ~code:(fun () -> Matrix.mult m1 m2) ~res:m1_times_m2;
     case_eps "mult2" ~code:(fun () -> Matrix.mult m2 m1) ~res:m2_times_m1;
     case_eps "mult3" ~code:(fun () -> Matrix.mult m3 m2) ~res:m3_times_m2;
     case_eps "mult4" ~code:(fun () -> Matrix.mult m1 v1) ~res:m1_times_v1;
     case_eps "mult5" ~code:(fun () -> Matrix.mult m3 v2) ~res:m3_times_v2;
     case_eps "scmult" ~code:(fun () -> Matrix.scmult sigma1 2.0) ~res:sigma1x2
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

let copy_batch = Test.create_batch "Copy vector"
    [
     case_eps "copy sigma1" ~code:(fun () -> Matrix.copy sigma1) ~res:sigma1;
     case_eps "cp vec sigma1 col" 
       ~code:(fun () -> 
         let m = Matrix.copy sigma1 in 
         let () = Matrix.copy_vec_mat_col m3_times_v2 m 0 in
         m)
       ~res:sigma1_cpcol_m3_times_v2;
     case_eps "cp vec sigma1 row" 
       ~code:(fun () -> 
         let m = Matrix.copy sigma1 in 
         let () = Matrix.copy_vec_mat_row m3_times_v2 m 1 in
         m)
       ~res:sigma1_cprow_m3_times_v2
    ]

(* Run all test batches *)
let () = 
  Test.runner [errors];
  Test.runner [creation; basic_ops; chol_batch; copy_batch]


