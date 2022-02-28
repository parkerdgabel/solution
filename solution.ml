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

let rest_after_whitspace s = 
  let start = String.index s ' ' |> Int.succ in
  String.sub s start (String.length s - start)

let compare_after_whitespace x y = 
  let x_after_whitepace = rest_after_whitspace x
  and y_after_whitepace = rest_after_whitspace y in
  compare x_after_whitepace y_after_whitepace

let combine_indices xs = List.mapi (fun i elem -> (i, elem)) xs

let findi_opt p xs =
  let xs_with_indices = combine_indices xs in
  match List.find_opt (fun (_, elem) -> p elem) xs_with_indices with
  | Some x -> Some x
  | None -> None

let pred_or_zero = function
| k when k <= 0 -> 0
| j -> Int.pred j

let findi elem l =
  let rec findi' elem i = function
  | [] -> failwith "findi"
  | x::xs -> if x = elem then i else findi' elem (i + 1) xs
  in
  findi' elem 0 l

let append_if_not_mem elem l = if List.mem elem l then l else l @ [elem]
 
let f accum top = 
  match List.find_opt (fun y -> compare_after_whitespace top y = -1) accum with
  | Some x -> let i = findi x accum in insert (pred_or_zero i) accum top
  | None -> append_if_not_mem top accum

let the_stuff_to_do x = 
  let stack = Stack.of_seq @@ List.to_seq x in
  Stack.fold f [] stack

let contains_whitespace = function
  | "" -> false
  | s -> String.contains s ' '

let do_things_and_stuff (x : string array) =
  let res = x |> Array.to_list |> List.filter contains_whitespace |> the_stuff_to_do in
  ()