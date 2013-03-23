(* 
   
   test.ml
   General utilities for testing.

   Andrei de A. Formiga, 2013-03-18

*)

type 'a test_case = 
    {
      cname: string;
      testcode: unit -> 'a;
      passtest: 'a -> bool;
    }

type 'a test_batch = 
    { 
      bname: string;
      cases: 'a test_case list;
    }


let create_case name ~code ~test = 
  { cname=name; testcode=code; passtest=test }

let create_case_eq name ~code ~res = 
  { cname=name; testcode=code; passtest=(fun r -> r = res)}

let create_case_exn name ~code exn = 
  { cname = name; 
    testcode = (fun () -> 
      try 
        let _ = code () in false 
      with e -> if e = exn then true else false); 
    passtest = (fun r -> r)}

let create_batch name cases = 
  { bname=name; cases=cases }

let run_case case = 
  let r = case.testcode () in
  case.passtest r 

let run_batch batch = 
  List.map (fun c -> (c.cname, run_case c)) batch.cases

let rec show_failures fails = 
  print_string "["; 
  List.iter (fun (cn, _) -> Printf.printf "%s " cn) fails;
  print_string "]"

let batch_runner batch = 
  Printf.printf "Running batch: %s ... " batch.bname;
  let size = List.length batch.cases in
  let results = run_batch batch in
  let failures = List.filter (fun (_, b) -> not b) results in
  match failures with
    [] -> Printf.printf "All pass (%d/%d)\n" size size
  | _ -> 
      let nfail = List.length failures in 
      Printf.printf "Pass: (%d/%d), Fail: (%d/%d), " (size-nfail) size nfail size;
      show_failures failures;
      print_newline ()

let runner bs = 
  let rec loop bs = 
    match bs with
      [] -> ()
    | b :: bs' -> ( batch_runner b; loop bs' ) in
  loop bs

