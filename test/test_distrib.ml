(* 

   test_distrib.ml
   Tests for distributions. 

   Andrei de A. Formiga, 2013-03-20

*)

(***

  TODO

    - statistical tests for goodness-of-fit of generated samples

***)

let test_mvnorm2 () = 
  let mu = Matrix.vector_from_array [| 1.0; 1.0 |] in
  let sigma = Matrix.identity 2 in
  let mvnorms1 = Distribution.samples_mvnorm mu sigma 8 in
  Matrix.print mvnorms1;
  Matrix.print_row_r mvnorms1 0;
  Matrix.print_row_r mvnorms1 1

let test_mvnorm3 () = 
  let mu = Matrix.vector_from_array [| 1.0; 1.0; 1.0 |] in
  let sigma = Matrix.identity 3 in
  let mvnorms1 = Distribution.sample_mvnorm mu sigma in
  Matrix.print mvnorms1 

let () = 
  test_mvnorm2 ();
  test_mvnorm3 ()

  
