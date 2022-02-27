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

let rest_after_whitspace s = match String.index_opt s ' ' with
| Some x -> let start = x + 1 in 
            Some (String.sub s start (String.length s - start))
| None -> None

let combine_indices xs = List.mapi (fun i elem -> (i, elem)) xs
let findi_opt p xs =
  let xs_with_indices = combine_indices xs in
  match List.find_opt (fun (_, elem) -> p elem) xs_with_indices with
  | Some x -> Some x
  | None -> None

let eq_after_whitespace e x = match rest_after_whitspace e with
| Some y -> y = x
| None -> false

let minus_one_or_zero = function
| k when k <= 0 -> 0
| j -> j - 1

let f (accum : string list) (top : string) = match rest_after_whitspace top with
| Some x -> (match findi_opt (fun e -> not (eq_after_whitespace e x)) accum with
            | Some (i, elem) -> if (String.length x) < (String.length elem) 
                                then insert (minus_one_or_zero (i - 1)) accum top
            | None -> accum @ [top])
| None -> accum

let the_stuff_to_do (x : string list) = 
  let stack = Stack.of_seq (List.to_seq x) in
  Stack.fold f [] stack

let do_things_and_stuff (x : string array) =
  let res = x |> Array.to_list |> the_stuff_to_do in
  ()