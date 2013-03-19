(* 
   
   test.ml
   General utilities for testing.

   Andrei de A. Formiga, 2013-03-18

*)

type ('a, 'b) test_case = 
    {
      cname: string;
      testcode: () -> 'a;
      passtest: 'a -> bool;
    }



