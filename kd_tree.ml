(* kd-tree: a tree to store k-dimensional points

   Cf. chapter 5.2 in:

   @book{CompGeomThirdEdSpringer,
   title     = "Computational Geometry: Algorithms and Applications",
   author    = "M. {de Berg} and O. Cheong and M. {van Kreveld} and
   M. Overmars",
   edition   = "Third Edition",
   pages     = {223--224},
   doi       = "10.1007/978-3-540-77974-2",
   year      = "2008",
   publisher = "Springer"
   } *)

module A = Array
module L = List

(* Functorial interface *)

module type Point = sig
  type t
  (* how many dimensions *)
  val ndims: int
  (* get coord in given dimension *)
  val get_coord: int -> t -> float
  (* point of coords *)
  val create: float array -> t
end

module Make = functor (P: Point) -> struct

  (* a k-dimensional box *)
  module Range = struct
    type t = { lb: P.t ; (* left bound *)
               rb: P.t } (* right bound *)
    let create lb rb =
      { lb; rb }
    (* get bounds in given dimension *)
    let get_bounds ndim r =
      (P.get_coord ndim r.lb, P.get_coord ndim r.rb)
    (* tighten lbound in given dimension *)
    let tighten_lbound ndim new_bound box =
      let new_coords = A.init P.ndims (fun i -> P.get_coord i box.lb) in
      new_coords.(ndim) <- new_bound;
      { box with lb = P.create new_coords }
    (* tighten rbound in given dimension *)
    let tighten_rbound ndim new_bound box =
      let new_coords = A.init P.ndims (fun i -> P.get_coord i box.rb) in
      new_coords.(ndim) <- new_bound;
      { box with rb = P.create new_coords }
    let min_corner () =
      P.create (A.init P.ndims (fun _ -> -.max_float))
    let max_corner () =
      P.create (A.init P.ndims (fun _ -> max_float))
    let max_box () =
      { lb = min_corner () ;
        rb = max_corner () }
    exception Outside_bounds
    let point_is_inside p box =
      try
        for i = 0 to P.ndims - 1 do
          let y = P.get_coord i p in
          let x, z = get_bounds i box in
          if y < x || y > z then raise Outside_bounds
        done;
        true
      with Outside_bounds -> false
    (* a small box is completely inside a big one if all its corners are inside *)
    let is_inside small big =
      (* FBR: I need a function to list all the corners of a box *)
      failwith "not implemented yet"
    exception Intersect
    (* two cubes intersect if any corner of any cube is inside the other cube *)
    let intersect small big =
      failwith "not implemented yet"
  end

  type t = Empty
         | Leaf of P.t
         | Node of t * float * t (* (left, med, right) *)

  let fcmp (x: float) (y: float): int =
    if x < y then -1
    else if x > y then 1
    else 0 (* x = y *)

  (* Bonus: the array is correctly sorted after! *)
  let median get_coord a =
    A.sort (fun p1 p2 -> fcmp (get_coord p1) (get_coord p2)) a;
    let n = A.length a in
    if n mod 2 = 1 then get_coord a.(n / 2)
    else 0.5 *. (get_coord a.(n / 2) +. get_coord a.((n / 2) - 1))

  exception Finished of int
  (* count how many times [p] is true from the left,
     abort as soon as it is false *)
  let left_count p a =
    let n = A.length a in
    try
      for i = 0 to n - 1 do
        if not (p (A.unsafe_get a i)) then
          raise (Finished i)
      done;
      n (* all OK *)
    with Finished i -> i (* some KO *)

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
        let ok_count = left_count (fun p -> P.get_coord dim p <= med) points in
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

  (* test if the tree invariant holds. If it doesn't, we are in trouble... *)
  let check t =
    let rec loop depth = function
      | Empty -> true
      | Leaf _ -> true
      | Node (left, med, right) ->
        let lefties = to_list left in
        let righties = to_list right in
        let dim = depth mod P.ndims in
        let depth' = depth + 1 in
        L.for_all (fun p -> P.get_coord dim p <= med) lefties &&
        L.for_all (fun p -> P.get_coord dim p > med) righties &&
        loop depth' left && loop depth' right in
    loop 0 t

  (* "orthogonal range query": return all points whose coordinates
     are inside [range] (a k-dimensional box) *)
  let search range t =
    let rec loop depth acc curr_box = function
      | Empty -> acc
      | Leaf p -> if Range.point_is_inside p range then p :: acc else acc
      | Node (left, med, right) ->
        let dim = depth mod P.ndims in
        let depth' = depth + 1 in
        let lbox = Range.tighten_rbound dim med curr_box in
        let acc' =
          if Range.is_inside lbox range then
            to_list_loop acc left
          else if Range.intersect lbox range then
            loop depth' acc lbox left
          else acc in
        let rbox = Range.tighten_lbound dim med curr_box in
        if Range.is_inside rbox range then
          to_list_loop acc' right
        else if Range.intersect rbox range then
          loop depth' acc' rbox right
        else acc' in
    let max_box = Range.max_box () in
    loop 0 [] max_box t

end
