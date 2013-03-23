(*

  distribution.ml
  Representation of probability distributions

  Andrei de A. Formiga, 2013-03-18

*)



type uni =
    Uniform of float * float  (* low, high *)
  | DUniform of int * int     (* low, high *)
  | Normal of float * float   (* mean, variance *)
  | Bernoulli of float        (* rate *)
  | Binomial of int * float   (* number, rate *)
  | Exp of float              (* parameter *)

type multi = 
    MVNormal of Matrix.t * Matrix.t  (* mean, covariance matrix *)

type t = Univariate of uni | Multivariate of multi

(* A uniform (pseudo-)Random Number Generator *)
type rng = {
    init: int -> unit;        (* initialize and set seed *)
    sample: float -> float;   (* uniform sampling on interval 0-param *)
    sample01: unit -> float;  (* uniform sampling on interval 0-1 *)
  }

(*
type sampler = {
    base: rng;
    sample: t -> float;
    samples: t -> int -> float list;
  }
*)

let pi = 4.0 *. (atan 1.0)

(** RNG from standard library (or Core) *)
let stdrng = { 
  init = Random.init; 
  sample = Random.float;
  sample01 = ( fun () -> Random.float 1.0 ); 
}

let sample_bernoulli ?(rng=stdrng) rate = 
  let u = rng.sample01 () in 
  if u <= rate then 1.0 else 0.0

let sample_exp ?(rng=stdrng) param =
  let u = rng.sample01 () in
  -. (log u) /. param   (* X = -log U / p is distributed as Exp(p) *)

let sample_stdnormal_boxmuller rng =
  let u1 = rng.sample01 () in
  let u2 = rng.sample01 () in
  let x1 = (sqrt (-.2.0 *. (log u1))) *. (cos (2.0 *. pi *. u2)) in
  let x2 = (sqrt (-.2.0 *. (log u1))) *. (sin (2.0 *. pi *. u2)) in
  (x1, x2)

let sample_stdnormal_boxmuller2 rng = 
  let rec attempt () = 
    let u1 = rng.sample01 () in
    let u2 = rng.sample01 () in
    let s = (u1 ** 2.0) +. (u2 ** 2.0) in
    if s <= 1.0 then (s, u1, u2) else attempt () in
  let s, u1, u2 = attempt () in
  let z = sqrt (-2.0 *. (log s) /. s) in
  let x1 = z *. u1 in 
  let x2 = z *. u2 in
  (x1, x2)

let sample_stdnormal_boxmuller3 rng = 
  let rec attempt () = 
    let y1 = sample_exp ~rng 1.0 in
    let y2 = sample_exp ~rng 1.0 in
    if y2 > ((1.0 -. y1) ** 2.0) /. 2.0 then y1 else attempt () in
  let y1 = attempt () in
  let u = rng.sample01 () in
  if u <= 0.5 then y1 else -.y1

let stdnorm_samples smp num =
  let res = Array.init num (fun i -> sample_stdnormal_boxmuller3 smp) in
  Matrix.vector_from_array res

let sample_uni ?(rng=stdrng) d = 
  match d with
    Uniform (low, high) -> let off = high -. low in (rng.sample off) +. off
  | Exp lambda -> sample_exp ~rng lambda 
  | Bernoulli rate -> sample_bernoulli ~rng rate
  | _ -> 0.0  (* FIX *)

(* TODO: move to matrix.ml *)
let r_output arr =
  print_string "c(";
  Array.iter (fun n -> print_float n; print_string ", ") arr;
  print_endline ")"

(** Sample from a multivariate normal distribution with mean vector 
    mu and covariance matrix sigma *)
let sample_mvnorm smp mu sigma =
  let n = Matrix.vector_size mu in         (* dimension of the MV normal *)
  let cholfact = Matrix.cholesky sigma in
  let nsamples = stdnorm_samples smp n in  (* stdnormal samples?? *)
  let res = Matrix.( cholfact * nsamples ) in
  Matrix.( res + mu )
