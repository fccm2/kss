type color_name =
  [ `blue | `blue_dir | `cyan | `dark_grey | `dark_red | `default | `green
  | `light_cyan | `magenta | `purple | `red | `test | `white | `yellow
  | `green2 | `green1 ]

let color_esc color_name s =
  let col_code = match color_name with
  | `blue_dir  -> "01;34"
  (*
  | `yellow    -> "01;33"
  *)
  | `dark_red  -> "02;31"
  | `purple    -> "03;35"
  | `dark_grey -> "01;30"
  | `test      -> "01;32"
  | `light_cyan -> "01;36"

  |     `red -> "35;49"
  |   `green -> "32;49"
  |   `green1 -> "32;49"
  |   `green2 -> "32;49"

  |  `yellow -> "33;49"
  (*
  |    `blue -> "34;49"
  | `magenta -> "31;49"
  |    `cyan -> "36;49"
  *)
  |    `blue -> "36;49"
  | `magenta -> "33;49"
  |    `cyan -> "36;49"
  |   `white -> "37;49"
  | `default -> "39;49"
  in
  Printf.sprintf "\027[%sm%s\027[00m" col_code s
;;

(*

# List.init;;
- : int -> (int -> 'a) -> 'a list = <fun>

# List.init 8 (fun _ -> '0') ;;
- : char list = ['0'; '0'; '0'; '0'; '0'; '0'; '0'; '0']

# List.nth;;
- : 'a list -> int -> 'a = <fun>

# List.nth ['0'; '0'; '0'; '0'; '0'; '0'; '0'; '0'] 3 ;;
- : char = '0'

*)

let list_replace_n lst n repl =
  let ln = List.length lst in
  let rec aux lst d ac =
    if d >= ln then invalid_arg "list_replace_n" else
    match lst with
    | _ :: tl when n = d -> (List.rev ac) @ (repl :: tl)
    | hd :: tl -> aux tl (d+1) (hd::ac)
    | [] -> failwith "list_replace_n"
  in
  aux lst 0 []

let list_replace_nxn lst d0 d1 repl =
  List.mapi (fun l0 line ->
    if l0 = d0 then list_replace_n line d1 repl else line
  ) lst

let list_access_n lst n =
  let ln = List.length lst in
  let rec aux lst d =
    if d >= ln then invalid_arg "list_access_n" else
    match lst with
    | hd :: _
      when n = d -> hd
    | _ :: tl -> aux tl (d+1)
    | [] -> failwith "list_access_n"
  in
  aux lst 0

let list_access_nxn lst d0 d1 =
  let lst1 = list_access_n lst d0 in
  let r = list_access_n lst1 d1 in
  r

let fold_left f ini lst =
  let rec aux fltr lst =
    match lst with
    | hd :: tl ->
        let fltr = f fltr hd in
        aux fltr tl
    | [] -> fltr
  in
  aux ini lst

let list_fold_left_i f ini lst =
  let rec aux d fltr lst =
    match lst with
    | hd :: tl ->
        let fltr = f d fltr hd in
        aux (d+1) fltr tl
    | [] -> fltr
  in
  aux 0 ini lst

let list_filter_fold_ij f lst =
  list_fold_left_i (fun i ac line ->
    list_fold_left_i (fun j ac elem ->
      if f elem then ((i, j), elem) :: ac else ac
    ) ac line
  ) [] lst

let list_filter_fold f lst =
  List.fold_left (fun ac line ->
    List.fold_left (fun ac elem ->
      if f elem then elem :: ac else ac
    ) ac line
  ) [] lst

module Lists : sig
  val replace_n : 'a list -> int -> 'a -> 'a list
  val replace_n2 : 'a list list -> int -> int -> 'a -> 'a list list
  val access_n : 'a list -> int -> 'a
  val access_n2 : 'a list list -> int -> int -> 'a
  val filter_xx : ('a -> bool) -> 'a list list -> 'a list
  val filter_xxi : ('a -> bool) -> 'a list list -> ((int * int) * 'a) list
  val print : (color_name * char) list list -> unit
end = struct
  let replace_n2 = list_replace_nxn ;;
  let replace_n = list_replace_n ;;
  let access_n = list_access_n ;;
  let access_n2 = list_access_nxn ;;
  let filter_xx = list_filter_fold ;;
  let filter_xxi = list_filter_fold_ij ;;
  let print lst =
    List.iteri (fun d0 line ->
      Printf.printf " %d - " d0;
      List.iter (fun (color_name, c) ->
        Printf.printf "  %s" (color_esc color_name (String.make 1 c));
      ) line;
      Printf.printf "\n%!" ;
    ) lst;
    Printf.printf "\n     " ;
    List.iter (fun _ ->
      Printf.printf "  !" ;
    ) (List.init 8 (fun _ -> '0'));
    Printf.printf "\n%!     " ;
    List.iteri (fun d0 _ ->
      Printf.printf "  %d" d0;
    ) (List.init 8 (fun _ -> '0'));
    Printf.printf "\n%!" ;
end

let () =
  let board = 
    List.init 8 (fun _ ->
      List.init 8 (fun _ -> `red, '_')
    )
  in
  let board = Lists.replace_n2 board 0 0 (`cyan, 'T') in
  let board = Lists.replace_n2 board 0 2 (`cyan, 'o') in
  let board = Lists.replace_n2 board 0 1 (`cyan, 'K') in
  let board = Lists.replace_n2 board 0 6 (`cyan, 'K') in
  let board = Lists.replace_n2 board 0 5 (`cyan, 'o') in
  let board = Lists.replace_n2 board 0 7 (`cyan, 'T') in
  let board = Lists.replace_n2 board 0 4 (`cyan, 'J') in
  let board = Lists.replace_n2 board 0 3 (`cyan, 'N') in

  let board = Lists.replace_n2 board 7 0 (`magenta, 'T') in
  let board = Lists.replace_n2 board 7 2 (`magenta, 'o') in
  let board = Lists.replace_n2 board 7 1 (`magenta, 'K') in
  let board = Lists.replace_n2 board 7 6 (`magenta, 'K') in
  let board = Lists.replace_n2 board 7 5 (`magenta, 'o') in
  let board = Lists.replace_n2 board 7 7 (`magenta, 'T') in
  let board = Lists.replace_n2 board 7 4 (`magenta, 'J') in
  let board = Lists.replace_n2 board 7 3 (`magenta, 'N') in

  let board = Lists.replace_n2 board 1 0 (`cyan, '.') in
  let board = Lists.replace_n2 board 1 2 (`cyan, '.') in
  let board = Lists.replace_n2 board 1 1 (`cyan, '.') in
  let board = Lists.replace_n2 board 1 6 (`cyan, '.') in
  let board = Lists.replace_n2 board 1 5 (`cyan, '.') in
  let board = Lists.replace_n2 board 1 7 (`cyan, '.') in
  let board = Lists.replace_n2 board 1 4 (`cyan, '.') in
  let board = Lists.replace_n2 board 1 3 (`cyan, '.') in

  let board = Lists.replace_n2 board 6 0 (`magenta, '.') in
  let board = Lists.replace_n2 board 6 2 (`magenta, '.') in
  let board = Lists.replace_n2 board 6 1 (`magenta, '.') in
  let board = Lists.replace_n2 board 6 6 (`magenta, '.') in
  let board = Lists.replace_n2 board 6 5 (`magenta, '.') in
  let board = Lists.replace_n2 board 6 7 (`magenta, '.') in
  let board = Lists.replace_n2 board 6 4 (`magenta, '.') in
  let board = Lists.replace_n2 board 6 3 (`magenta, '.') in
  let initial_board = board in

  let turn = (`magenta, `cyan) in
  let swap_turn (a, b) = (b, a) in
  let played d (a, b) =
    if d = a then (a, b) else (b, a)
  in
  let _ = swap_turn in
  let _ = played in
  let take_first lst =
    match lst with
    | frst :: _ -> frst
    | [] -> invalid_arg "take_first"
  in
  let rand_take lst =
    let ln = List.length lst in
    let n = Random.int ln in
    List.nth lst n
  in
  Random.self_init () ;
  let strait_0 = [ ( 1, 0); ( 2, 0); ( 3, 0); ( 4, 0); ( 5, 0); ( 6, 0); ( 7, 0); ] in
  let strait_1 = [ (-1, 0); (-2, 0); (-3, 0); (-4, 0); (-5, 0); (-6, 0); (-7, 0); ] in
  let strait_2 = [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7); ] in
  let strait_3 = [ (0, -1); (0, -2); (0, -3); (0, -4); (0, -5); (0, -6); (0, -7); ] in

  let diago_0 = [ (-1, -1); (-2, -2); (-3, -3); (-4, -4); (-5, -5); (-6, -6); (-7, -7); ] in
  let diago_1 = [ ( 1, -1); ( 2, -2); ( 3, -3); ( 4, -4); ( 5, -5); ( 6, -6); ( 7, -7); ] in
  let diago_2 = [ ( 1,  1); ( 2,  2); ( 3,  3); ( 4,  4); ( 5,  5); ( 6,  6); ( 7,  7); ] in
  let diago_3 = [ (-1,  1); (-2,  2); (-3,  3); (-4,  4); (-5,  5); (-6,  6); (-7,  7); ] in

  let knite_0 = [ (-2, -1); ] in
  let knite_1 = [ (-2, 1); ] in
  let knite_2 = [ (2, -1); ] in
  let knite_3 = [ (2, 1); ] in

  let knite_4 = [ (-1, -2); ] in
  let knite_5 = [ (-1, 2); ] in
  let knite_6 = [ (1, -2); ] in
  let knite_7 = [ (1, 2); ] in

  let probs = [
    'T', 5;
    'K', 7;
    'o', 17;
    'J', 1;
    'N', 1;
    '.', 5;
  ] in

  let dests_1 = [
    '.', [ [ (-1, 0); ] ];
    'K', [ knite_0; knite_1; knite_2; knite_3; knite_4; knite_5; knite_6; knite_7; ];
    'o', [ diago_0; diago_1; diago_2; diago_3];
    'J', [
      [ (1, 0) ];
      [ (0, -1) ];
      [ (-1, 0) ];
      [ (0, 1) ];

      [ (1, 1) ];
      [ (1, -1) ];
      [ (-1, 1) ];
      [ (-1, -1) ];
    ];
    'T', [
      strait_0;
      strait_2;
      strait_3;
      strait_1;
    ];
    'N', [ strait_0; strait_2; strait_3; strait_1;
           diago_0; diago_1; diago_2; diago_3; ];
  ] in
  let dests_0 = [
    '.', [ [ (1, 0); ] ];
    'K', [ knite_0; knite_1; knite_2; knite_3; knite_4; knite_5; knite_6; knite_7; ];
    'o', [ diago_0; diago_1; diago_2; diago_3 ];
    'J', [ [ (1, 0) ]; [ (0, -1) ]; [ (-1, 0) ]; [ (0, 1) ];
           [ (1, 1) ]; [ (1, -1) ]; [ (-1, 1) ]; [ (-1, -1) ];
    ];
    'T', [ strait_0; strait_2; strait_3; strait_1; ];
    'N', [ diago_0; diago_1; diago_2; diago_3;
           strait_0; strait_2; strait_3; strait_1 ];
  ] in
  let dests = [
    `magenta, dests_1;
    `cyan, dests_0;
  ] in

  let unstack stack =
    match stack with
    | prev_board :: past_boards -> (prev_board, past_boards)
    | [] -> (initial_board, [])
  in

  Lists.print board;
  let rec play color turn board stack =
    let a, b = turn in
    let color_orig = color in (* prev *)
    let color = if color = a then b else a in (* cur. *)
    let elems = Lists.filter_xxi (fun (col, _) -> col = color) board in 
    List.iter (fun ((i, j), (col, char)) ->
      Printf.printf "# %d %d : %c\n%!" i j char;
    ) elems;
    let is_dot ((_, _), (_, char)) = (char = '.') in
    let char_elem ((_, _), (_, char)) = char in
    let color_elem ((_, _), (color, _)) = color in
    let _ = is_dot in
    (*
    let elems = List.fold_left (fun ac elem -> if is_dot elem then elem::ac else elem::elem::ac ) [] elems in
    *)

    let elems = List.map (fun elem -> let prob = List.assoc (char_elem elem) probs in (prob, elem) ) elems in
    let elems = List.map (fun (prob, elem) -> (Random.int prob, elem)) elems in

    begin
      List.iter (fun (prb, elem) ->
        Printf.printf "# %d %s\n%!" prb (color_esc (color_elem elem) (String.make 1 (char_elem elem)));
      ) elems;
    end;

    let elems = List.sort (fun (p0, _) (p1, _) -> compare p0 p1 ) elems in
    let elems = List.rev elems in

    (*
    # List.sort (fun a b -> compare a b)   [1; 2; 3; 5; 10; 4; 13; 0; 17];;

      - : int list = [0; 1; 2; 3; 4; 5; 10; 13; 17]
    *)

    Printf.printf "#--\n%!";
    begin
      List.iter (fun (prb, elem) ->
        Printf.printf "# %d %s\n%!" prb (color_esc (color_elem elem) (String.make 1 (char_elem elem)));
      ) elems;
    end;
    let ln = List.length elems in
    let l1 = Random.int ln in
    let l2 = if l1 = 0 then 0 else Random.int l1 in
    let l3 = if l2 = 0 then 0 else Random.int l2 in
    let l4 = if l3 = 0 then 0 else Random.int l3 in

    let elem = List.nth elems l4 in
    let _, elem = elem in

    let s = Printf.sprintf "> %d %d %d %d : %s" l1 l2 l3 l4 (String.make 1 (char_elem elem)) in
    Printf.printf "%s\n%!" (color_esc (color_elem elem) s);

    let _ = take_first in
    (*
    let elem = rand_take elems in
    *)
    (*
    let elem = take_first elems in
    *)
    (*
    let _, elem = elem in
    *)

    let board =
      match elem with
      | ((i, j), (color, char)) ->
          (*
          let shar = color_esc color (String.make 1 char) in
          Printf.printf "# %d %d : %s\n%!" i j shar;
          *)
          let s = Printf.sprintf "# %d %d : %c" i j char in
          Printf.printf "%s\n%!" (color_esc color  s);
          let dests = List.assoc color dests in
          begin
            match color, char with
            | `cyan, char
            | `magenta, char ->
                (*
                let rec faild board =  in faild board
                *)

                let dirs = List.assoc char dests in

                let dirs = rand_take dirs in
                List.iter (fun (x, y) -> Printf.printf "; %d %d" x y) dirs;
                Printf.printf "\n%!";

                (*
                List.iter (fun (x, y) ->
                  try let color, piece = Lists.access_n2 board (x+i) (y+j) in (*access*)
                    Printf.printf "> %d %d %s\n%!" x y (color_esc color (String.make 1 piece));
                  with _ -> Printf.printf "> %d %d _\n%!" x y ;
                ) dirs;
                *)

                let dirs =
                  List.filter (fun (x, y) ->
                    let x, y = (i+x, j+y) in
                    (x >=0 && y >=0 && x <=7 && y <=7)
                  ) dirs
                in

                (*
                List.iter (fun (x, y) ->
                  let color, piece = Lists.access_n2 board (x+i) (y+j) in (*access*)
                  Printf.printf "; %d %d %s\n%!" x y (color_esc color (String.make 1 piece));
                ) dirs;
                *)
                
                (*
                Printf.printf "# t:%d\n%!" (List.length dirs);
                *)

                (*no-dirs*)
                if (List.length dirs) = 0 then begin Printf.printf "# re-play1\n%!" ; play color_orig turn board stack ; end;
    
                (*
                let dir =
                  match char with
                  | '.' -> rand_take dirs
                  | 'K' -> rand_take dirs
                  | _ -> rand_take dirs
                  (*
                  | _ -> (0, 0)
                  *)
                in
                *)
                let rec troug prev dirs ac = (*search-fwrd*)
                  match dirs with
                  | dir :: dirs ->
                      let x, y = dir in
                      let i, j = (i+x, j+y) in
                      let c, piece = Lists.access_n2 board i j in (*access*)
                      if List.mem piece ['T'; 'o'; 'K'; 'J'; 'N'; '.'] && c = color (*blck-d*)
                      then begin
                        match prev with
                        | Some _ -> ac
                        | None -> Printf.printf "# re-play0\n%!" ; []
                      end
                      else troug (Some(i, j)) dirs ((i, j)::ac)
                  | [] ->
                      if (List.length ac) = 0 then
                      begin
                        Printf.printf "# blk-r\n%!"; (*try-agan*)
                        []
                      end
                      else ac
                in
                let dirs = troug None dirs [] in

                if dirs = [] then begin
                  play color_orig turn board stack ; 
                end;

                let dir = rand_take dirs in

                let board = Lists.replace_n2 board i j (`red, '_') in (*repl-orig*)
                (*
                let mv (x, y) (i, j) = (x + i, y + j) in
                let i, j = mv (i, j) dir in
                *)
                let i, j = dir in
                let s = Printf.sprintf "# %d %d" i j in
                Printf.printf "%s\n%!" (color_esc color  s);

                begin
                  let color_dest, piece = Lists.access_n2 board i j in (*access*)
                  if color_dest = color then (*dont eat it-self*)
                  begin
                    Printf.printf "# dt-eat\n%!";
                    play color_orig turn board stack ;
                  end;
                  match piece with
                  | '_' -> ()
                  | 'T' | 'o' | 'K' | 'J' | 'N' | '.' ->
                    if Random.int 100 < 75 then () else (*dont-eat*)
                    begin
                      Printf.printf "# vd-eat\n%!";
                      play color_orig turn board stack ; (*try-agan*)
                    end;
                  | _ -> invalid_arg "un-known"
                end;

                let board = Lists.replace_n2 board i j (color, char) in
                board

            | _ ->
                board
          end;
    in
    Lists.print board;
    run turn None board stack

  and run turn pos board stack =
    let usr = input_line stdin in
    if List.mem usr ["exit"; ""] then exit 0 ;
    if List.mem usr ["prev"; "previous"] then begin
      let prev_board, past_board = unstack stack in
      Lists.print prev_board;
      run turn pos (prev_board) (past_board) ;
    end;
    let c0 = String.get usr 0 in
    let c1 = String.get usr 1 in
    let c0 = int_of_string (String.make 1 c0) in
    let c1 = int_of_string (String.make 1 c1) in
    begin if c0 > 7 || c1 > 7 then run turn None board stack end;
    Printf.printf "# %d %d\n%!" c0 c1;
    match pos with
    | Some (d0, d1) ->
        let (color, cb)  = Lists.access_n2 board d0 d1 in
        let color = match color with `green1 -> `magenta | `green2 -> `cyan | _ -> exit 1 in
        let board = Lists.replace_n2 board d0 d1 (`red, '_') in
        let board = Lists.replace_n2 board c0 c1 (color, cb) in
        Printf.printf "# req-mv: %d %d -> %d %d\n%!" d0 d1 c0 c1;
        Lists.print board;
        play color turn board stack

    | None ->
        let prev_board = board in
        let (color, ca)  = Lists.access_n2 board c0 c1 in
        begin if ca = '_' then run turn None board stack end;
        let green = match color with `magenta -> `green1 | `cyan -> `green2 | _ -> exit 1 in
        let board = Lists.replace_n2 board c0 c1 (green, ca) in
        Lists.print board;
        run turn (Some (c0, c1)) board (prev_board::stack)
  in
  try run turn None board [board]
  with End_of_file -> print_newline ()
;;

