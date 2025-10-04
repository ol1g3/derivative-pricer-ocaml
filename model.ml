open Pricer
open Base

(** Model interface for stochastic models *)
module type Model = sig 
  val r : float  (** Risk-free rate *)
  val sigma: float  (** Volatility *)
  val generate_path : float -> int -> float array
  val run : int -> int -> float -> analytics_result
end

(** Black-Scholes model with geometric Brownian motion *)
module BlackScholes : Model = struct
  let r = 0.05      (* 5% risk-free rate *)
  let sigma = 0.2   (* 20% volatility *)

  (** Generate price path using geometric Brownian motion *)
  let generate_path initial_price n_steps =
    let path = Array.create ~len:(n_steps + 1) initial_price in
    let dt = 1.0 /. Float.of_int n_steps in
    
    for i = 0 to n_steps - 1 do
      let prev_price = path.(i) in
      let normal = Pricer.standard_normal () in
      (* Black-Scholes SDE: dS = (r - σ²/2)S dt + σS dW *)
      let drift = (r -. sigma *. sigma /. 2.0) *. dt in
      let diffusion = sigma *. Float.sqrt dt *. normal in
      path.(i + 1) <- prev_price *. Float.exp (drift +. diffusion)
    done;
    path

  (** Run Monte Carlo simulation for asset price estimation *)
  let run n_simulations n_steps initial_price =
    if n_simulations <= 0 || n_steps <= 0 then
      failwith "Invalid parameters: all must be positive"
    else
      let start_time = Unix.gettimeofday () in
      
      (* Generate final prices from multiple paths *)
      let results = Array.init n_simulations ~f:(fun _ ->
        let path = generate_path initial_price n_steps in
        path.(Array.length path - 1)
      ) in
      
      let end_time = Unix.gettimeofday () in
      
      (* Calculate statistics *)
      let price = Array.fold results ~init:0.0 ~f:(+.) /. Float.of_int n_simulations in
      let variance = Array.fold results ~init:0.0 ~f:(fun acc x -> 
        acc +. (x -. price) *. (x -. price)
      ) /. Float.of_int n_simulations in
      let std_dev = Float.sqrt variance in
      
      (* 95% confidence interval *)
      let conf_interval = (
        (price -. 1.96 *. std_dev /. Float.sqrt (Float.of_int n_simulations)),
        (price +. 1.96 *. std_dev /. Float.sqrt (Float.of_int n_simulations))
      ) in
      
      { price; std_dev; conf_interval; time_taken = end_time -. start_time }
end

(** Main execution: Price American call option with Monte Carlo *)
let () = 
  Random.init 42;  (* Set seed for reproducibility *)
  
  (* Simulation parameters *)
  let n_simulations = 1000 in
  let n_steps = 100 in
  let starting_price = 100.0 in
  let maturity = 1.0 in
  let eps = 1.0 in

  (* Price American call option *)
  let module M = BlackScholes in
  let contract = AmericanContract (starting_price, Call) in
  let analytics = Pricer.price_contract (module M) contract starting_price n_simulations n_steps maturity in
  
  (* Calculate delta (price sensitivity) *)
  let delta = Pricer.delta (module M) contract starting_price n_simulations n_steps maturity eps in
  
  (* Output results *)
  Stdio.printf "Monte Carlo Price: %f ± %f (95%% CI: [%f, %f])\n" 
    analytics.price 
    (analytics.std_dev /. Float.sqrt (Float.of_int n_simulations)) 
    (fst analytics.conf_interval) 
    (snd analytics.conf_interval);
  Stdio.printf "Delta: %f\n" delta;
  Stdio.printf "Time Taken: %f seconds\n" analytics.time_taken;