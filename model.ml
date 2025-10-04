(* open Owl *)
open Pricer

module type Model = sig 
  val r : float
  val sigma: float

  val path_statistics : float array -> (float * float * float) (* mean, max, min *)
  val run : int -> int -> float -> float -> unit
end

module BlackScholes : Model = struct
  let r = 0.05
  let sigma = 0.2

  let default_params = {
    nu = r -. (sigma *. sigma) /. 2.0;
    rho = 0.0;
    vega = sigma;
  }

  let path_statistics path =
    let mean = Array.fold path ~init:0.0 ~f:(+.) /. Float.of_int (Array.length path) in
    let max_price = Array.fold path ~init:Float.neg_infinity ~f:Float.max in
    let min_price = Array.fold path ~init:Float.infinity ~f:Float.min in
    (mean, max_price, min_price)

  let run n_simulations n_steps initial_price initial_volatility =
    Random.init (Int.of_float (Unix.time ()));
    let initial_price = Float.log initial_price in
    let results = Array.init n_simulations ~f:(fun _ -> calculate_price default_params initial_price initial_volatility (generate_normals n_steps)) in
    let results_antithetic = Array.init n_simulations ~f:(fun _ -> simulate_with_antithetic default_params initial_price initial_volatility n_steps) in
    
    let average = Array.fold results ~init:0.0 ~f:(+.) /. Float.of_int n_simulations in
    let variance = Array.fold results ~init:0.0 ~f:(fun acc x -> acc +. (x -. average) *. (x -. average)) /. Float.of_int n_simulations in
    Stdio.printf "Plain Monte Carlo:\n";
    Stdio.printf "Average Result: %f\n" average;
    Stdio.printf "Variance: %f\n" variance;

    let average_antithetic = Array.fold results_antithetic ~init:0.0 ~f:(+.) /. Float.of_int n_simulations in
    let variance_antithetic = Array.fold results_antithetic ~init:0.0 ~f:(fun acc x -> acc +. (x -. average_antithetic) *. (x -. average_antithetic)) /. Float.of_int n_simulations in
    Stdio.printf "\nAntithetic Variates:\n";
    Stdio.printf "Average Result: %f\n" average_antithetic;
    Stdio.printf "Variance: %f\n" variance_antithetic;
    Stdio.printf "Variance Reduction Factor: %f\n" (variance /. variance_antithetic);

    let (avg, max_val, min_val) = path_statistics results in
    Stdio.printf "Path statistics: avg: %f, min: %f, max: %f\n" avg min_val max_val;
    let (avg, max_val, min_val) = path_statistics results_antithetic in
    Stdio.printf "Path statistics: avg: %f, min: %f, max: %f\n" avg min_val max_val
end

let () = 
  let module M = BlackScholes in
  M.run 1000 100 10.0 0.2