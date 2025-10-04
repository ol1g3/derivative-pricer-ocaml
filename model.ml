(* open Owl *)
module type Model = sig 
  val r : float
  val sigma: float
  
  val generate_path_plain : float -> int -> float array
  val generate_path_antithetic : float -> int -> float array
end
