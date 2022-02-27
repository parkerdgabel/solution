let rec take k xs = match k with
    | 0 -> []
    | k -> match xs with
           | [] -> failwith "take"
           | y::ys -> y :: (take (k - 1) ys)

let rec drop k xs = match k with
| 0 -> xs
| k -> match xs with
        | [] -> failwith "drop"
        | _::ys -> drop (k - 1) ys

let rest_after_whitspace i s = 
  let start = i + 1 in 
  String.sub s start (String.length s - start) 

let f (accum : string list) (top : string) = match String.index_opt top ' ' with
| Some x -> let top_after_whitespace = rest_after_whitspace x top in
            List.

| None -> accum

let the_stuff_to_do (x : string list) = 
  let stack = Stack.of_seq (List.to_seq x) in
  Stack.fold f [] stack

let do_things_and_stuff (x : string array) =
  let res = x |> Array.to_list |> the_stuff_to_do in
  ()