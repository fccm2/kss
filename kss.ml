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
  | `magenta -> "31;49"
  |    `cyan -> "34;49"
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

module Lists : sig
  val replace_n : 'a list -> int -> 'a -> 'a list
  val replace_n2 : 'a list list -> int -> int -> 'a -> 'a list list
  val access_n : 'a list -> int -> 'a
  val access_n2 : 'a list list -> int -> int -> 'a
  val print : (color_name * char) list list -> unit
end = struct
  let replace_n2 = list_replace_nxn ;;
  let replace_n = list_replace_n ;;
  let access_n = list_access_n ;;
  let access_n2 = list_access_nxn ;;
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

  Lists.print board;
  let rec run pos board =
    let usr = input_line stdin in
    if List.mem usr ["exit"; ""] then exit 0 ;
    (*
    print_endline usr;
    *)
    let c0 = String.get usr 0 in
    let c1 = String.get usr 1 in
    let c0 = int_of_string (String.make 1 c0) in
    let c1 = int_of_string (String.make 1 c1) in
    begin if c0 > 7 || c1 > 7 then run None board
    end;
    Printf.printf "# %d %d\n%!" c0 c1;
    match pos with
    | Some (d0, d1) ->
        let (color, cb)  = Lists.access_n2 board d0 d1 in
        let board = Lists.replace_n2 board d0 d1 (`red, '_') in
        let board = Lists.replace_n2 board c0 c1 (color, cb) in
        Printf.printf "# req-mv: %d %d -> %d %d\n%!" d0 d1 c0 c1;
        Lists.print board;
        run None board

    | None ->
      let (color, ca)  = Lists.access_n2 board c0 c1 in
      begin if ca = '_' then run None board
      end;
      begin
        match ca with
        | 'T' -> Printf.printf "# 0 +1 \n";
        | 'K' -> Printf.printf "# +2 +1 \n";
        | _ -> ()
      end;
      let green = match color with `magenta -> `green1 | `cyan -> `green2 | _ -> exit 1 in
      let board = Lists.replace_n2 board c0 c1 (green, ca) in
      Lists.print board;
      run (Some (c0, c1)) board
  in
  try run None board
  with End_of_file -> print_newline ()
;;

