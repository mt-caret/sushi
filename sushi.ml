open Core
open Async
open Gnuplot

let log_end_at_n p n =
  Float.of_int (n - 1) *. Float.log p +. Float.log (1. -. p)
;;

let log_prob p xs =
  List.sum (module Float) ~f:(log_end_at_n p) xs
;;

let range left right n i =
  let open Float.O in
  left + (right - left) * (of_int i) / (of_int n)
;;

let extract_samples ~in_filename =
  let read reader = Reader.lines reader |> Pipe.to_list in
  begin match in_filename with
    | None -> Lazy.force Reader.stdin |> read
    | Some filename -> Reader.with_file filename ~f:read
  end
    >>| List.map ~f:Int.of_string
;;

let plot ~left ~right ~n ~in_filename ~out_filename () =
  let%map samples = extract_samples ~in_filename in
  let results =
    List.init n ~f:(range left right n)
      |> List.map ~f:(fun p -> (p, log_prob p samples))
  in
  let gp = Gp.create () in
  Series.points_xy results
    |> Gp.plot ~output:(Output.create (`Png out_filename)) gp
;;

let rec ternary_search ~f n left right =
  let diff = right -. left in
  let mid1 = left +. diff /. 3. in
  let mid2 = left +. diff *. 2. /. 3. in
  let (y1, y2) = (f mid1, f mid2) in
  let (left, right) = if y1 < y2 then (mid1, right) else (left, mid2) in
  if n = 0 then (left +. right) /. 2. else ternary_search ~f (n - 1) left right
;;

let mle_calc ~in_filename () =
  let%map samples = extract_samples ~in_filename in
  ternary_search 1000 0. 1. ~f:(fun p -> log_prob p samples)
    |> printf "MLE: %f\n"
;;

let input_filename_param =
  let open Command.Param in
  flag "in-filename" (optional file) ~doc:" input (defaults to stdin)"
;;

let plot_command =
  let open Command.Let_syntax in
  let%map_open left = flag "left" (optional_with_default 0. float) ~doc:" left bound"
  and right = flag "right" (optional_with_default 1. float) ~doc:" right bound"
  and n = flag "n" (optional_with_default 100 int) ~doc:" number of data points"
  and in_filename = input_filename_param
  and out_filename = flag "out-filename" (optional_with_default "out.png" file) ~doc:" where to save plot (png)"
  in
  plot ~left ~right ~n ~in_filename ~out_filename
;;

let mle_calc_command =
  let open Command.Let_syntax in
  let%map_open in_filename = input_filename_param in
  mle_calc ~in_filename
;;

let () =
  let commands =
    [ ("plot", plot_command, "plot distribution of p")
    ; ("mle", mle_calc_command, "estimate p with MLE")
    ]
      |> List.map ~f:(fun (name, cmd, summary) ->
        (name, Command.async cmd ~summary))
  in
  Command.group ~summary:"sushida calculator" commands
    |> Command.run
;;
