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

    ignore (init_pair 2 Color.white Color.blue);
    wattron w (WA.color_pair 2 lor WA.bold);

    Array.iteri (fun i ch -> ignore (mvwaddch w 0 (i + 1) ch)) chtype_array;

    wattroff w (WA.color_pair 2 lor WA.bold);
    ignore (wrefresh w)

  let new_win w h : window =
    let w = newwin w h 1 1 in
    box w 0 0;

    ignore (wrefresh w);
    w

  let display_dirs (w : window) a idx =
    werase w;
    box w 0 0;

    ignore (init_pair 3 Color.blue Color.black);
    Array.iteri
      (fun i x ->
        let str_x =
          Array.to_list x
          |> List.map (fun ch -> Char.escaped (char_of_int ch))
          |> String.concat ""
        in
        if i <> idx then ignore (mvwaddstr w (i + 1) 1 str_x)
        else (
          wattron w (WA.color_pair 3 lor WA.bold);
          String.iteri
            (fun j ch -> ignore (mvwaddch w (i + 1) (j + 1) (int_of_char ch)))
            str_x;
          wattroff w (A.color_pair 3 lor WA.bold)))
      a;

    ignore (wrefresh w)

  let rec main_loop (w : window) (i : int) =
    let c = wgetch w in
    match char_of_int c with
    | 'q' -> end_window w
    | 'j' ->
        let dirs = FILES.get_directories () in
        display_dirs w dirs (i + 1);
        box w 0 0;
        set_dir_title w (Sys.getcwd ());
        ignore (wrefresh w);
        main_loop w (i + 1)
    | 'k' ->
        let dirs = FILES.get_directories () in
        display_dirs w dirs (i - 1);
        box w 0 0;
        set_dir_title w (Sys.getcwd ());
        ignore (wrefresh w);
        main_loop w (i - 1)
    | 'l' ->
        let current_dir = Sys.getcwd () in
        let dirs = Sys.readdir current_dir in
        if i >= 0 && i < Array.length dirs then
          Sys.chdir (Filename.concat current_dir dirs.(i));
        let new_dirs = FILES.get_directories () in
        display_dirs w new_dirs 0;
        box w 0 0;
        set_dir_title w (Sys.getcwd ());
        ignore (wrefresh w);
        main_loop w 0
    | 'h' ->
        Sys.chdir Filename.parent_dir_name;
        let dirs = FILES.get_directories () in
        display_dirs w dirs 1;
        box w 0 0;
        set_dir_title w (Sys.getcwd ());
        ignore (wrefresh w);
        main_loop w 0
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
          ignore (init_pair 1 Curses.Color.red, Curses.Color.red);
          ignore (refresh ());
          Some win
        with ex ->
          end_window win;
          raise ex)
    | None -> None
end
