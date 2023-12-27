open Ui.UI
open Curses
open Ui.FILES

let () =
  let win = init_window () in
  match win with
  | Some win ->
      let w = new_win 0 0 in
      let dirs = get_directories () in
      display_dirs w dirs 1;
      box w 0 0;
      set_dir_title w (Sys.getcwd ());
      main_loop w;
      end_window win
  | None -> endwin ()
