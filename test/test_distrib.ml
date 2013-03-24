(* 

   test_distrib.ml
   Tests for distributions. 

   Andrei de A. Formiga, 2013-03-20

*)

(***

  TODO

    - statistical tests for goodness-of-fit of generated samples

***)

let id2 = Matrix.identity 2

let sigma1 = Matrix.from_array 2 2
  [| 1.0; 0.5; 
     0.5; 2.0  |]

let test_mvnorm2 () = 
  let mu = Matrix.vector_from_array [| 1.0; 1.0 |] in
  let mvnorms1 = Distribution.samples_mvnorm mu id2 90 in
  Matrix.print_row_r mvnorms1 0;
  Matrix.print_row_r mvnorms1 1

let test_mvnorm22 () = 
  let mu = Matrix.vector_from_array [| 0.6; 1.2 |] in
  let sigma = Matrix.scmult id2 0.5 in
  let mvnorms1 = Distribution.samples_mvnorm mu sigma 90 in
  Matrix.print_row_r mvnorms1 0;
  Matrix.print_row_r mvnorms1 1

let test_mvnorm23 () = 
  let mu = Matrix.vector_from_array [| 1.2; 0.7 |] in
  let mvnorms1 = Distribution.samples_mvnorm mu sigma1 90 in
  Matrix.print_row_r mvnorms1 0;
  Matrix.print_row_r mvnorms1 1

let test_mvnorm3 () = 
  let mu = Matrix.vector_from_array [| 1.0; 1.0; 1.0 |] in
  let sigma = Matrix.identity 3 in
  let mvnorms1 = Distribution.sample_mvnorm mu sigma in
  Matrix.print mvnorms1 

let () = 
  test_mvnorm2 ();
  test_mvnorm22 ();
  test_mvnorm23 ();
  test_mvnorm3 ()

  
