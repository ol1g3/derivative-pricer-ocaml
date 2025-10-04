open Base

(** Analytics result structure for pricing statistics *)
type analytics_result = {
  price: float;
  std_dev: float;
  conf_interval: (float * float);
  time_taken: float;
}

(** Model interface for stochastic models *)
module type Model = sig 
  val r : float
  val sigma : float
  val generate_path : float -> int -> float array
  val run : int -> int -> float -> analytics_result
end

type option_type = Call | Put
type contract = 
  | VanillaContract of float * option_type 
  | AsianContract of float * option_type 
  | AmericanContract of float * option_type

module Pricer = struct
  (** Generate standard normal random variable using Box-Muller transform *)
  let standard_normal () : float =
    let u1 = Random.float 1.0 in
    let u2 = Random.float 1.0 in
    Float.sqrt (-2.0 *. Float.log u1) *. Float.cos (2.0 *. Float.pi *. u2)

  (** Generate array of normal random variables *)
  let generate_normals n : float array =
    Array.init n ~f:(fun _ -> standard_normal ())

  (** Calculate payoff for different contract types *)
  let payoff (contract : contract) final_price path =
    match contract with
    | VanillaContract (strike, Call) -> Float.max 0. (final_price -. strike)
    | VanillaContract (strike, Put) -> Float.max 0. (strike -. final_price)
    | AsianContract (strike, Call) -> 
        let avg_price = Array.fold path ~init:0.0 ~f:(+.) /. Float.of_int (Array.length path) in
        Float.max 0. (avg_price -. strike)
    | AsianContract (strike, Put) ->
        let avg_price = Array.fold path ~init:0.0 ~f:(+.) /. Float.of_int (Array.length path) in
        Float.max 0. (strike -. avg_price)
    | AmericanContract (strike, Call) ->
        (* Maximum payoff achievable at any point along the path *)
        Array.fold path ~init:0.0 ~f:(fun max_payoff spot_price ->
          Float.max max_payoff (Float.max 0.0 (spot_price -. strike))
        )
    | AmericanContract (strike, Put) ->
        Array.fold path ~init:0.0 ~f:(fun max_payoff spot_price ->
          Float.max max_payoff (Float.max 0.0 (strike -. spot_price))
        )

  (** Price contract using Monte Carlo simulation *)
  let price_contract (module M : Model) contract initial_price n_simulations n_steps maturity =
    let start_time = Unix.gettimeofday () in
    
    (* Generate payoffs from multiple simulation paths *)
    let results = Array.init n_simulations ~f:(fun _ ->
      let path = M.generate_path initial_price n_steps in
      let final_price = path.(Array.length path - 1) in
      payoff contract final_price path
    ) in
    
    let end_time = Unix.gettimeofday () in
    
    (* Calculate discounted option price *)
    let average_payoff = Array.fold results ~init:0.0 ~f:(+.) /. Float.of_int n_simulations in
    let price = average_payoff *. Float.exp (-. M.r *. maturity) in
    
    (* Calculate statistics for confidence intervals *)
    let variance = Array.fold results ~init:0.0 ~f:(fun acc x -> 
      acc +. (x -. average_payoff) *. (x -. average_payoff)
    ) /. Float.of_int n_simulations in
    let std_dev = Float.sqrt variance in
    
    (* 95% confidence interval *)
    let conf_interval = (
      (price -. 1.96 *. std_dev /. Float.sqrt (Float.of_int n_simulations)),
      (price +. 1.96 *. std_dev /. Float.sqrt (Float.of_int n_simulations))
    ) in
    
    { price; std_dev; conf_interval; time_taken = end_time -. start_time }

  (** Calculate delta using finite differences *)
  let delta (module M : Model) contract initial_price n_simulations n_steps maturity epsilon =
    (* Price at S + ε and S - ε *)
    let price_up = price_contract (module M) contract (initial_price +. epsilon) n_simulations n_steps maturity in
    let price_down = price_contract (module M) contract (initial_price -. epsilon) n_simulations n_steps maturity in
    (* Central difference approximation: Δ ≈ (P(S+ε) - P(S-ε)) / 2ε *)
    (price_up.price -. price_down.price) /. (2.0 *. epsilon)
end