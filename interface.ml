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
  let (stream_type, in_str, do_show_tree) = 
    let arg_len = Array.length Sys.argv in
      if  arg_len = 1 then
        (CHANNEL, "", false)
      else if arg_len = 3 then (
        let is_tree =
          if Sys.argv.(2) = "true" then
            true
          else
            false
        in
        (STRING, Sys.argv.(1), is_tree)
      ) else 
        (STRING, Sys.argv.(1), false)
  in
  let tokens =
    get_tokens in_str stream_type
  in
  let (lm, _) =  parse (List.fold_left (fun x y -> y::x) [] tokens)
      (Lambda(["paren"],[])) (Lambda([], [])) false in 
  let (lm', do_trans) = beta lm false do_show_tree in
  let lm'' = (reduce_paren lm') in
    if do_show_tree then (
      show_lm_native lm'' default_indent
    );
    print_endline "";
    print_string "result -> ";
    print_endline (string_of_int (calc_lm lm'' "f" 0));
    print_endline "";


