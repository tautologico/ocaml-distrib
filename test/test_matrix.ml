(*

  test_matrix.ml
  Tests of the matrix and vector operations. 

  Andrei de A. Formiga, 2013-03-17

*)

open Test

(* Tests for simple operations *)
let m1 = Matrix.from_array 2 2
  [| 1.0; 2.0;
     1.5; 3.0 |]

let m2 = Matrix.from_array 2 2
  [| 2.0; 3.0;
     2.5; 4.0 |]

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
  


let ops_c1 = { 
  cname = "test_add"; 
  testcode = (fun () -> Matrix.add m1 m2);
  passtest = (Matrix.eq_eps m1_plus_m2);
}

let ops_c2 = { 
  cname = "test_sub";
  testcode = (fun () -> Matrix.sub m2 m1);
  passtest = (Matrix.eq_eps m2_minus_m1);
}

let ops_c3 = {
  cname = "test_mult1";
  testcode = (fun () -> Matrix.mult m1 m2);
  passtest = (Matrix.eq_eps m1_times_m2);
}

let ops_c4 = {
  cname = "test_mult2";
  testcode = (fun () -> Matrix.mult m2 m1);
  passtest = (Matrix.eq_eps m2_times_m1);
}

let basic_ops = {
  bname = "Basic operations";
  cases = [ops_c1; ops_c2; ops_c3; ops_c4];
}

let () = 
  runner [basic_ops]
