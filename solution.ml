module String = struct
  include String
  let rest_after_whitspace s = 
    let start = Int.succ @@ String.index s ' ' in
    String.sub s start (String.length s - start)
  
  let compare_after_whitespace x y = 
    let x_after_whitepace = rest_after_whitspace x
    and y_after_whitepace = rest_after_whitspace y in
    compare x_after_whitepace y_after_whitepace

  let contains_whitespace = (Fun.flip String.contains) ' '

  let ltaw x y = (compare_after_whitespace x y) = -1
end

module Int = struct
  include Int

  let pred_or_zero = function
    | k when k <= 0 -> 0
    | j -> Int.pred j
end

module List = struct
  include List

  let findi elem l =
    let rec findi' elem i = function
    | [] -> failwith "findi"
    | x::xs -> if x = elem then i else findi' elem (i + 1) xs
    in
    findi' elem 0 l

  let rec take k xs = match k with
    | 0 -> []
    | k -> match xs with
           | [] -> failwith "take"
           | y::ys -> y :: (take (k - 1) ys)

  let rec drop k xs = match k with
  | 0 -> xs
  | k -> match xs with
        | [] -> []
        | _::ys -> drop (k - 1) ys

  let insert i xs elem = (take i xs) @ [elem] @ (drop i xs)

  let insert_before elem x l = 
    let i = findi x l in 
    insert (Int.pred_or_zero i) l elem
  
  let append_if_not_mem elem l = if List.mem elem l then l else l @ [elem]
end
 
let f accum top = 
  match List.find_opt (fun y -> String.ltaw top y) accum with
  | Some x -> List.insert_before top x accum
  | None -> List.append_if_not_mem top accum

let the_stuff_to_do x = 
  let stack = Stack.of_seq @@ List.to_seq x in
  Stack.fold f [] stack

  (* Note that the original function mutated an array in place 
    This function does as well *)
let do_things_and_stuff (x : string array) =
  let res = x 
          |> Array.to_list 
          |> List.filter String.contains_whitespace 
          |> the_stuff_to_do 
          |> List.rev 
          |> Array.of_list in
  Array.iter print_endline res