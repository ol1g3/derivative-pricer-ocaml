open Base

module Pricer = struct
  type params = {
    nu : float;
    rho : float;
    vega : float;
  }

  let default_params = {
    nu = 0.0001;
    rho = 0.1;
    vega = 0.001;
  }

  let standard_normal () : float =
    let u1 = Random.float 1.0 in
    let u2 = Random.float 1.0 in
    Float.sqrt (-2.0 *. Float.log u1) *. Float.cos (2.0 *. Float.pi *. u2)

  let generate_normals n : float list =
    List.init n ~f:(fun _ -> standard_normal ())

  let calculate_price (params : params) (initial_price : float) (initial_volatility : float) (normals : float list) : float =
    let rec loop price volatility = function
      | [] -> price
      | normal :: rest ->
        let next_volatility = params.rho *. volatility +. params.vega *. normal in
        let next_price = price +. params.nu +. (Float.exp volatility) *. normal in
        loop next_price next_volatility rest
    in
    loop initial_price initial_volatility normals

  let simulate_with_antithetic (params : params) (initial_price : float) (initial_volatility : float) (n_steps : int) : float =
    let normals = generate_normals n_steps in
    let antithetic_normals = List.map normals ~f:(Float.neg) in
    let price1 = calculate_price params initial_price initial_volatility normals in
    let price2 = calculate_price params initial_price initial_volatility antithetic_normals in
    (price1 +. price2) /. 2.0

  let run () =
    let n_simulations = 10000 in
    let n_steps = 100 in
    let initial_price = Float.log 10.0 in
    let initial_volatility = 0.0 in
    let results = List.init n_simulations ~f:(fun _ -> simulate_with_antithetic default_params initial_price initial_volatility n_steps) in
    let average = List.fold results ~init:0.0 ~f:(+.) /. Float.of_int n_simulations in
    Stdio.printf "Average Result: %f\n" average
end

let () =
  let module P = Pricer in
  P.run ()