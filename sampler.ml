(*

  sampler.ml
  Sampling for probability distributions

*)

type distribution =
  Uniform of float * float  (* low, high *)
| DUniform of int * int     (* low, high *)
| Gaussian of float * float (* mean, variance *)
| Bernoulli of float        (* rate *)
| Binomial of int * float   (* number, rate *)
| Exp of float              (* parameter *)

type basic_sampler =
  {
    init: int -> unit;
    sample_unif: float -> float;
    sample_unif1: unit -> float;
  }

type sampler =
  {
    base: basic_sampler;
    sample: distribution -> float;
    samples: distribution -> int -> float list;
  }

let pi = 4.0 *. (atan 1.0)

(** Sampler from standard library (or Core) *)
let stdsmp = { init = Random.init; sample_unif = Random.float;
               sample_unif1 = ( fun () -> Random.float 1.0 ); }

let sample_exp smp param =
  let u = smp.sample_unif1 () in
  -. (log u) /. param   (* X = -log U / p is distributed as Exp(p) *)

let sample_stdnormal_boxmuller smp =
  let u1 = smp.sample_unif1 () in
  let u2 = smp.sample_unif1 () in
  let x1 = (sqrt (-.2.0 *. (log u1))) *. (cos (2.0 *. pi *. u2)) in
  let x2 = (sqrt (-.2.0 *. (log u1))) *. (sin (2.0 *. pi *. u2)) in
  (x1, x2)

let stdnorm_samples smp num =
  let res = Array.init num (fun i -> sample_stdnormal_boxmuller smp) in
  res


let r_output arr =
  print_string "c(";
  Array.iter (fun n -> print_float n; print_string ", ") arr;
  print_endline ")"
