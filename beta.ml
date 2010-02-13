type char_type = LPAREN | RPAREN | LAMBDA | DOT | OTHER
type stream_type = STRING | CHANNEL
type lambda_v = string list
type tree = App of lambda_v | Lambda of lambda_v * tree list

let channel = open_in "/home/sode/prog/ocaml/lambda.lm"
let token_buf = Buffer.create 64

(* utilities *)
let get_l (a, b) = a

let get_r (a, b) = b

let rec each f e = fun x ->
  match x with
      [] -> e
    | x::rest -> (
        f x;
        each f e rest
      )

let rec contain list n =
  match list with
      x::rest -> 
        if x = n then true
        else contain rest n
    | [] -> false

let empty lambda =
  match lambda with 
      App([]) -> true
    | Lambda([], []) -> true
    | _ -> false

let default_indent = "  "

let rec show_list_native list =
  match list with
      [] -> ()
    | x::rest ->
        print_string (x ^ " ");
        show_list_native (rest)

let rec show_lm_native lm indent =
  match lm with 
      App(l) ->
        print_string (indent ^ "App:");
        show_list_native l;
        print_string "\n"
    | Lambda(l, list) ->
        print_string (indent ^ "Lambda \n");
        print_string (indent ^ " Variables:");
        show_list_native l;
        print_string "\n";
        each (fun x -> show_lm_native x (indent ^ default_indent)) () list

let rec show_list list =
  match list with
      [] -> ()
    | "paren"::rest -> show_list rest
    | x::rest ->
        print_string (x ^ " ");
        show_list (rest)

let rec show_lm lm indent =
  match lm with 
      App(l) ->
        show_list l;
    | Lambda(l, list) ->
        match l with
            [] ->
              each (fun x -> show_lm x (indent ^ default_indent)) () list
          | ["paren"] -> (
              print_string "(";
              each (fun x -> show_lm x (indent ^ default_indent)) () list;
              print_string ")"
            )
          | _ -> (
              print_string "(\\";
              show_list l;
              print_string ". ";
              each (fun x -> show_lm x (indent ^ default_indent)) () list;
              print_string ")" 
            )

let get_v_lm lm = 
  match lm with 
      Lambda(v, l) -> v
    | App(v) -> [""]

let get_children_lm lm = 
  match lm with
      Lambda(v, l) -> l
    | App(v) -> []

let add_lm_list_pre lm new_lm =
  if not(empty new_lm) then
    match lm with 
        Lambda([], []) -> new_lm
      | Lambda(l, r) -> Lambda(l, [new_lm]@r)
      | _ -> lm
  else
    lm

let add_variable_list_pre x value =
  match x with
      Lambda(l, r) -> Lambda(value::l, r)
    | _ -> x

let add_lm_list lm new_lm =
  if not(empty new_lm) then
    match lm with 
        Lambda([], []) -> new_lm
      | Lambda(l, r) -> Lambda(l, r@[new_lm])
      | _ -> lm
  else
    lm

let add_variable_list lm value =
  match lm with
      Lambda(l, r) -> Lambda(l@[value], r)
    | _ -> lm

let rec reduce_paren lm =
  let rec reduce_paren_list lmlist =
    match lmlist with
        [] -> lmlist
      | lm1::lmlist1 ->
          (reduce_paren lm1)::(reduce_paren_list lmlist1)
  in
    match lm with
        App(v) -> lm
      | Lambda(["paren"], lmlist') -> (
          match lmlist' with
              [Lambda(["paren"], lmlist'')] ->
                Lambda(["paren"], (reduce_paren_list lmlist''))
            | [Lambda(vlist, lmlist''')] ->
                Lambda(vlist, (reduce_paren_list lmlist'''))
            | _ -> 
                Lambda(["paren"], (reduce_paren_list lmlist'))
        )
      | Lambda(vlist, lmlist') ->
          Lambda(vlist, (reduce_paren_list lmlist'))

(* get token from lambda string *)
let check_type str =
  match str with
      "(" -> LPAREN
    | ")" -> RPAREN
    | "\\" -> LAMBDA
    | "." -> DOT
    | _ -> OTHER

let get_tokens in_str stream_type =
  let get_a_char idx =
    if stream_type = STRING then
      in_str.[idx]
    else
      input_char channel
  in
  let add_token token_buf token tokens =
    if Buffer.length token_buf != 0 then (
      Buffer.clear token_buf;
      token::tokens
    ) else (
      tokens
    )
  in
  let rec get_char in_token tokens idx =
    try
      let c = get_a_char idx in
        match check_type (String.make 1 c) with
            LPAREN|RPAREN|LAMBDA|DOT ->
              let token = Buffer.contents token_buf in
                get_char false ((String.make 1 c)::(add_token token_buf token tokens)) (idx + 1)
          | OTHER -> 
              match c with 
                  '\r' | '\n' | '\t' | ' ' -> 
                    let token = Buffer.contents token_buf in
                      get_char false (add_token token_buf token tokens) (idx + 1)
                | _ -> 
                    if in_token then (
                      Buffer.add_char token_buf c;
                      get_char false ((Buffer.contents token_buf)::tokens) (idx + 1)
                    ) else (
                      Buffer.add_char token_buf c;
                      get_char true tokens (idx + 1)
                    )
    with
        End_of_file -> tokens
      | Invalid_argument in_str -> tokens
  in
    get_char false [] 0

(* parse. create Lambda tree *)
let rec parse tokens lm cur_lm is_lambda =
  match tokens with
      [] -> (
        match cur_lm with
            Lambda([], []) -> (lm, [])
          | App(a) -> ((add_lm_list lm cur_lm), [])
          | _ -> (lm, [])
      )
    | token::rest -> 
        match check_type token with
            LPAREN ->
              let (lm', next) = parse rest (Lambda([], [])) (Lambda(["paren"], [])) false in
                parse next (add_lm_list (add_lm_list lm cur_lm) lm') (Lambda([], [])) false
          | LAMBDA ->
              parse rest lm cur_lm true 
          | DOT ->
              parse rest lm cur_lm false
          | RPAREN ->
              ((add_lm_list lm cur_lm), rest)
          | OTHER ->
              if is_lambda then 
                parse rest lm (add_variable_list cur_lm token) is_lambda
              else 
                parse rest lm (add_lm_list cur_lm (App([token]))) is_lambda

(*  Alpha Conversion *)
let rec get_replaced_v v depth =
  if depth = 0 then v
  else get_replaced_v (v  ^ "0") (depth - 1)

let rec convert_vlist list x replace =
  match list with
      [] -> []
    | y::rest ->
        if x == y then replace::rest
        else y::(convert_vlist rest x replace)

let rec alpha_convert x lmlist depth do_replace =
  let replace_lm src replace lm depth =
    match lm with
        App(vlist) ->
          if contain vlist src then
            (App(convert_vlist vlist src replace), true)
          else
            (lm, false)
      | Lambda(vlist, lmlist') ->
          let (lmlist'', do_replace'') = (alpha_convert x lmlist' (depth + 1) do_replace) in
            if do_replace'' then
              (Lambda((convert_vlist vlist x (get_replaced_v src depth)), lmlist''), true)
            else
              (Lambda(vlist, lmlist''), false)              
  in
    match lmlist with
        [] -> ([], do_replace)
      | lm::lmlist_rest ->
          let replace = get_replaced_v x depth in
          let (lm', do_replace') = replace_lm x replace lm depth in
            if do_replace then
              (lm'::(get_l(alpha_convert x lmlist_rest depth do_replace)), true)
            else
              let (lmlist', do_replace'') = (alpha_convert x lmlist_rest depth do_replace) in
                (lm::lmlist', do_replace'')

let rec alpha = fun lm ->
  match lm with
      App(v) -> lm
    | Lambda(vlist, lmlist) ->
        match vlist with
            ["paren"]|[] -> 
              print_endline "alpha1";
              each (fun x -> alpha x) (Lambda([],[])) lmlist
          | x::vlist_rest ->
              print_endline "alpha2";
              let (lmlist', do_replace) = alpha_convert x lmlist 0 false in
                if do_replace then (
                  print_endline "alpha2 suc";
                  Lambda(vlist, lmlist')
                ) else (
                  add_variable_list_pre (alpha (Lambda(vlist_rest, lmlist))) x
                )

(* Beta Reduction *)
let rec beta_reduction = fun lm ->
  let rec trans_lms lmlist x lm do_trans = (* lmlist u App:f do_trans *)
    match lmlist with
        [] -> ([], do_trans)
      | lm'::rest ->
          match lm' with
              App(vlist) -> 
                if (contain vlist x) then (
                  let (lmlist1, do_trans1) = (trans_lms rest x lm true) in
                    ((lm::lmlist1), true)
                ) else (
                  let (lmlist1, do_trans1) = (trans_lms rest x lm do_trans) in
                    (lm'::lmlist1, true)
                )
            | Lambda(l, r) -> 
                if (contain l x) then (* for alpha conversion *) (
                  (lmlist, do_trans)
                )
                else
                  let (lmlist1, do_trans1) = (trans_lms r x lm do_trans) in
                  let (lmlist2, do_trans2) = (trans_lms rest x lm do_trans) in
                    (Lambda(l, lmlist1)::lmlist2, (do_trans1 || do_trans2))
  in

  let rec trans_lm lm1 lm2 do_trans =
    let rec trans_lmlist lmlist lm2 do_trans =
      match lmlist with
          [] -> (lmlist, false)
        | lm::rest ->
            let (lm', do_trans') = trans_lm lm lm2 do_trans in
            let (lmlist', do_trans'') = trans_lmlist rest lm2 do_trans in
              (lm'::lmlist', (do_trans'||do_trans'')) 
    in
      match lm1 with
          App(vlist) -> (lm1, false)
        | Lambda(vlist, lmlist) ->
            match vlist with
                [] -> (lm1, false)
              | ["paren"] ->
                  let (lm'', do_trans'') = beta_reduction lm1 in (*trans_lmlist lmlist lm2 do_trans in*)
                    if do_trans'' then 
                      (Lambda(["paren"], lm''::lm2::[]), true) 
                    else
                      let (lmlist', do_trans''') = trans_lmlist lmlist lm2 do_trans in
                        if do_trans''' then
                          (Lambda(["paren"], lmlist'), true)
                        else 
                          (lm1, false)              
              | [v1] ->
                  let (lmlist', do_trans') = trans_lms lmlist v1 lm2 false in
                    if do_trans' then
                      (Lambda(["paren"], lmlist'), true)
                    else
                      (lm1, false)
              | "paren"::v2::rest ->
                  let (lmlist', do_trans') = trans_lms lmlist v2 lm2 false in
                    if do_trans' then 
                      (Lambda("paren"::rest, lmlist'), true)
                    else 
                      (lm1, false)
              | v1::rest ->
                  let (lmlist', do_trans') = trans_lms lmlist v1 lm2 false in
                    if do_trans' then
                      (Lambda(rest, lmlist'), true)
                    else
                      (lm1, false)
  in

  match lm with
      App(vlist) -> (lm, false)
    | Lambda([], []) -> (lm, false)
    | Lambda(vlist, lmlist) ->
        match lmlist with
            [] -> (lm, false)
          | [lm'] -> 
              let (lm', do_trans') = beta_reduction lm' in
                if do_trans' then
                  (Lambda(vlist, [lm']), true)
                else
                  (lm, false)
          | lm1::lm2::rest ->
              let (lm'', do_trans) = trans_lm lm1 lm2 false in
              let lmlist' = 
                if (get_v_lm lm'') = ["paren"] then (
                  get_children_lm lm''
                ) else
                  [lm'']
              in
                if do_trans then
                  (Lambda(vlist, lmlist'@rest), true)
                else
                  let (lm1', do_trans') = beta_reduction lm1 in
                    if do_trans' then
                      (Lambda(vlist, lm1'::lm2::rest), true)
                    else
                      let (lm''', do_trans'') = beta_reduction (Lambda(vlist, lm2::rest)) in
                        if do_trans'' then
                          (Lambda(vlist, lm1::(get_children_lm lm''')), true)
                        else
                          (lm, false)

let rec beta lm do_trans =
  let (lm', do_trans') = (beta_reduction lm) in
    show_lm lm' default_indent;
    print_endline "";
    if do_trans' then (
      beta (reduce_paren lm') do_trans' 
    )
    else 
      (lm', false)
