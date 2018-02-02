(* kd-tree for d-dimensional points *)

module A = Array
module L = List

(* Functorial interface *)

module type Point = sig
  type t
  (* how many dimensions *)
  val ndims: int
  (* get coord in given dimension *)
  val get_coord: int -> t -> float
end

module Make = functor (P: Point) -> struct

  type t = Empty
         | Leaf of P.t
         | Node of t * float * t (* (left, med, right) *)

  let fcmp (x: float) (y: float): int =
    if x < y then -1
    else if x > y then 1
    else 0 (* x = y *)

  (* the array is correctly sorted after *)
  let median get_coord a =
    A.sort (fun p1 p2 -> fcmp (get_coord p1) (get_coord p2)) a;
    let n = A.length a in
    if n mod 2 = 1 then get_coord a.(n / 2)
    else 0.5 *. (get_coord a.(n / 2) +. get_coord a.((n / 2) - 1))

  exception Finished of int
  (* count how many times [p] is true from the left,
     stop counting as soon as it is false *)
  let left_count p a =
    let n = A.length a in
    try
      for i = 0 to n - 1 do
        if not (p (A.unsafe_get a i)) then
          raise (Finished i)
      done;
      n
    with Finished i -> i

  let partition ok_count a =
    let n = A.length a in
    let left = A.sub a 0 ok_count in
    let right = A.sub a ok_count (n - ok_count) in
    (left, right)

  let create (all_points: P.t array) =
    let rec loop depth points =
      let n = A.length points in
      if n = 0 then Empty
      else if n = 1 then Leaf (A.unsafe_get points 0)
      else
        let dim = depth mod P.ndims in
        let med = median (P.get_coord dim) points in
        let ok_count = left_count (fun p -> P.get_coord dim <= med) points in
        let left, right = partition ok_count points in
        let depth' = depth + 1 in
        Node (loop depth' left, med, loop depth' right)
    in loop 0 all_points

  (* to_list with an acc *)
  let rec to_list_loop acc = function
    | Empty -> acc
    | Leaf p -> p :: acc
    | Node (left, _, right) ->
      let acc' = to_list_loop acc right in
      to_list_loop acc' left

  let to_list t =
    to_list_loop [] t

  let is_empty = function
    | Empty -> true
    | _ -> false

  (* return all points whose coordinates are inside [range] *)
  let search range tree =
    failwith "not implemented yet"

  (* test if the tree invariant holds.
     If it doesn't, we are in trouble... *)
  let rec check =
    failwith "not implemented yet"

end
