(*

  test_matrix.ml
  Tests of the matrix and vector operations. 

  Andrei de A. Formiga, 2013-03-17

*)

type ('a, 'b) test_case = 
{
  cname: string;
  input: 'a;
  output: 'b;
}

type ('a, 'b) test_batch = 
{
  bname: string;
  cases: ('a, 'b) test_case list;
}

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
  
let test_cholesky () = 
  let test_case name input output = 
    let res = cholesky input in
    Matrix.eq_eps res output in
  test_case "sigma1" sigma1 cholesky_sigma1


let unpack_inputs x = x

let test_basic_ops () = 
  let test_add_case case = 
    let in1, in2 = case.input in
    let res = Matrix.add in1 in2 in
    Matrix.eq_eps res case.output in
  0
