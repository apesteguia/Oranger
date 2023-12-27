open Curses

module UI = struct
  let safe_initscr () =
    let win = initscr () in
    if win = null_window then None else Some win

  let end_window (w : window) =
    let _ = delwin w in
    endwin ()

  let init_window () =
    match safe_initscr () with
    | Some win -> (
        try
          ignore (keypad win true);
          ignore (cbreak ());
          ignore (noecho ());
          ignore (start_color ());
          ignore (curs_set 1);
          ignore (init_pair 1 Curses.Color.white, Curses.Color.black);
          ignore (refresh ());
          Some win
        with ex ->
          end_window win;
          raise ex)
    | None -> None
end

(*
void
init_window ()
{
    initscr ();
    keypad (stdscr, TRUE);
    cbreak ();
    noecho ();
    start_color ();
    curs_set (1);
    init_pair (1, COLOR_WHITE, COLOR_BLACK);
    refresh ();
}
void
end_window (WINDOW *win)
{
    delwin (win);
    endwin ();
}
*)
