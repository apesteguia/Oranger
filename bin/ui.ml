open Curses

module FILES = struct
  let format_to_chtype (str : string) : chtype array =
    let ch_array =
      Array.init (String.length str) (fun i ->
          char_of_int (int_of_char str.[i]))
    in
    let chtype_array = Array.map (fun ch -> int_of_char ch) ch_array in
    chtype_array

  let get_directories () =
    let dirs = Sys.readdir (Sys.getcwd ()) in
    Array.map (fun x -> format_to_chtype x) dirs
end

module UI = struct
  let safe_initscr () =
    let win = initscr () in
    if win = null_window then None else Some win

  let end_window (w : window) =
    ignore (delwin w);
    endwin ()

  let set_dir_title w title =
    let ch_array =
      Array.init (String.length title) (fun i ->
          char_of_int (int_of_char title.[i]))
    in

    let chtype_array = Array.map (fun ch -> int_of_char ch) ch_array in

    ignore (mvwaddchstr w 0 1 chtype_array);
    ignore (wrefresh w)

  let new_win w h : window =
    let w = newwin w h 1 1 in
    box w 0 0;

    ignore (wrefresh w);
    w

  let display_dirs (w : window) a idx =
    wclear w;
    Array.iteri
      (fun i x ->
        if i <> idx then ignore (mvwaddchstr w (i + 1) 1 x)
        else (
          wattron w (A.color_pair 2);
          ignore (mvwaddchstr w (i + 1) 1 x);
          wattroff w (A.color_pair 2)))
      a;

    ignore (wrefresh w)

  let rec main_loop (w : window) =
    let c = wgetch w in
    match char_of_int c with
    | 'q' -> end_window w
    | 'l' -> end_window w
    | 'h' ->
        Sys.chdir Filename.parent_dir_name;
        let dirs = FILES.get_directories () in
        display_dirs w dirs 1;
        box w 0 0;
        set_dir_title w (Sys.getcwd ());
        ignore (wrefresh w);
        main_loop w
    | _ -> end_window w

  let init_window () =
    match safe_initscr () with
    | Some win -> (
        try
          ignore (keypad win true);
          ignore (cbreak ());
          ignore (noecho ());
          ignore (start_color ());
          ignore (curs_set 0);
          ignore (init_pair 1 Curses.Color.red, Curses.Color.black);
          ignore (init_pair 2 Curses.Color.green, Curses.Color.black);
          ignore (refresh ());
          Some win
        with ex ->
          end_window win;
          raise ex)
    | None -> None
end
