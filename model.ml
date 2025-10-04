(* open Owl *)
open Pricer

module type Model = sig 
  val r : float
  val sigma: float
  val generate_path : float -> int -> float array
  val path_statistics : float array -> (float * float * float) (* mean, max, min *)
  val run : int -> int -> float -> float -> analytics_result
end

module BlackScholes : Model = struct
  let r = 0.05
  let sigma = 0.2

  let path_statistics path =
    let mean = Array.fold path ~init:0.0 ~f:(+.) /. Float.of_int (Array.length path) in
    let max_price = Array.fold path ~init:Float.neg_infinity ~f:Float.max in
    let min_price = Array.fold path ~init:Float.infinity ~f:Float.min in
    (mean, max_price, min_price)

  let generate_path initial_price n_steps =
    let path = Array.create ~len:(n_steps + 1) initial_price in
    let dt = 1.0 /. Float.of_int n_steps in
    for i = 0 to n_steps - 1 do
      let prev_price = path.(i) in
      let normal = Pricer.standard_normal () in
      let drift = (r -. sigma *. sigma /. 2.0) *. dt in
      let diffusion = sigma *. Float.sqrt dt *. normal in
      path.(i + 1) <- prev_price *. Float.exp (drift +. diffusion)
    done;
    path

  let run n_simulations n_steps initial_price initial_volatility =
    if n_simulations <= 0 || n_steps <= 0 then
      failwith "Invalid parameters: all must be positive"
    else
      Random.init (Int.of_float (Unix.time ()));
      let start_time = Unix.gettimeofday () in
      let results = Array.init n_simulations ~f:(fun _ ->
        let path = generate_path initial_price n_steps in
        path.(Array.length path - 1)
      ) in
      let end_time = Unix.gettimeofday () in
      let price = Array.fold results ~init:0.0 ~f:(+.) /. Float.of_int n_simulations in
      let variance = Array.fold results ~init:0.0 ~f:(fun acc x -> acc +. (x -. price) *. (x -. price)) /. Float.of_int n_simulations in
      let std_dev = Float.sqrt variance in
      let conf_interval = ((price -. 1.96 *. std_dev /. Float.sqrt (Float.of_int n_simulations)),
                         (price +. 1.96 *. std_dev /. Float.sqrt (Float.of_int n_simulations))) in
      { price; std_dev; conf_interval; time_taken = end_time -. start_time }

end

let () = 
  let module M = BlackScholes in
  let contract = AmericanContract (100.0, Call) in
  let analytics = Pricer.price_contract (module M) contract 100.0 10000 100 1.0 in
  let delta = Pricer.delta (module M) contract 100.0 10000 100 1.0 0.01 in
  Stdio.printf "Monte Carlo Price: %f ± %f (95%% CI: [%f, %f])\n" analytics.price (analytics.std_dev /. Float.sqrt (Float.of_int 10000)) (fst analytics.conf_interval) (snd analytics.conf_interval);
  Stdio.printf "Delta: %f\n" delta;
  Stdio.printf "Time Taken: %f seconds\n" analytics.time_taken