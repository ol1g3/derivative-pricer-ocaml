open Base

module Pricer = struct
  type params = {
    nu : float;
    rho : float;
    vega : float;
  }

  let standard_normal () : float =
    let u1 = Random.float 1.0 in
    let u2 = Random.float 1.0 in
    Float.sqrt (-2.0 *. Float.log u1) *. Float.cos (2.0 *. Float.pi *. u2)

  let generate_normals n : float array =
    Array.init n ~f:(fun _ -> standard_normal ())

  let calculate_price (params : params) (initial_price : float) (initial_volatility : float) (normals : float array) : float =
    let rec loop price volatility i =
      if i >= Array.length normals then price
      else
        let normal = normals.(i) in
        let next_volatility = params.rho *. volatility +. params.vega *. normal in
        let next_price = price +. params.nu +. (Float.exp volatility) *. normal in
        loop next_price next_volatility (i + 1)
    in
    loop initial_price initial_volatility 0

  let simulate_with_antithetic (params : params) (initial_price : float) (initial_volatility : float) (n_steps : int) : float =
    let normals = generate_normals n_steps in
    let antithetic_normals = Array.map normals ~f:(Float.neg) in
    let price1 = calculate_price params initial_price initial_volatility normals in
    let price2 = calculate_price params initial_price initial_volatility antithetic_normals in
    (price1 +. price2) /. 2.0
end