open Ui.UI
open Curses
open Ui.FILES

let () =
  let win = initscr () in
  ignore (cbreak ());
  ignore (noecho ());
  ignore (start_color ());
  ignore (curs_set 0);
  ignore (init_pair 1 Curses.Color.red, Curses.Color.blue);
  ignore (refresh ());

  let maxy, maxx = getmaxyx win in
  let w = newwin maxx maxy 0 0 in

  ignore (wrefresh w);
  let dirs = get_directories () in
  display_dirs w dirs 0;
  set_dir_title w (Sys.getcwd ());
  main_loop w 0;
  end_window win
