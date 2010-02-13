open Beta

(* For church number *)
let rec calc_lm lm f count = 
  let rec calc_lm_list lmlist f count = 
    match lmlist with
        [] -> count
      | lm::rest ->
          (calc_lm lm f count) + (calc_lm_list rest f count)
  in
  match lm with
      App(vlist) ->
        if vlist = [f] then
          count + 1
        else
          count
    | Lambda(vlist, lmlist) ->
        calc_lm_list lmlist f count

let _ =
  let (stream_type, in_str) = 
    if Array.length Sys.argv > 1 then
      (STRING, Sys.argv.(1))
    else
      (CHANNEL, "")
  in
  let tokens =
    get_tokens in_str stream_type
  in
  let (lm, _) =  parse (List.fold_left (fun x y -> y::x) [] tokens)
      (Lambda(["paren"],[])) (Lambda([], [])) false in 
  let (lm', do_trans) = beta lm false in
  let lm'' = (reduce_paren lm') in
    print_endline "";
    print_string "result -> ";
    print_endline (string_of_int (calc_lm lm'' "f" 0));
    print_endline "";


