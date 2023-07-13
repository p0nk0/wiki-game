open! Core

module Position = struct
  module T = struct
    type t = int * int * char [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let get_pos (row, col, _) = row, col
end

module Maze = struct
  type t =
    { maze : char array array
    ; width : int
    ; height : int
    }
  [@@deriving compare, sexp]

  let of_file input_file =
    let start_pos = ref (0, 0, ' ') in
    let end_pos = ref (0, 0, ' ') in
    let res =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.map ~f:String.to_array
      |> List.to_array
    in
    Array.iteri res ~f:(fun i arr ->
      Array.iteri arr ~f:(fun j char ->
        if Char.equal char 'S' then start_pos := i, j, 'S';
        if Char.equal char 'E' then end_pos := i, j, 'E'));
    let width = Array.length res in
    let height = Array.length res.(1) in
    if Position.equal !start_pos (0, 0, ' ')
       || Position.equal !end_pos (0, 0, ' ')
    then failwith "couldn't find start / end position"
    else { maze = res; width; height }, !start_pos, !end_pos
  ;;

  let adjacent_positions maze ((row, col, _) : Position.t) =
    let in_bounds row col =
      0 < row && row < maze.width && 0 < col && col < maze.height
    in
    [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
    |> List.filter ~f:(fun (row, col) ->
         in_bounds row col
         && (Char.equal maze.maze.(row).(col) '.'
             || Char.equal maze.maze.(row).(col) 'E'))
  ;;
end

let get_cell (maze : Maze.t) ((row, col) : int * int) : Position.t =
  row, col, maze.maze.(row).(col)
;;

let solve_maze maze start_pos end_pos =
  let visited = ref Position.Set.empty in
  let rec dfs path v =
    visited := Set.add !visited v;
    Maze.adjacent_positions maze v
    |> List.filter ~f:(fun v -> not (Set.mem !visited (get_cell maze v)))
    |> List.fold ~init:None ~f:(fun acc v ->
         if not (is_none acc)
         then acc
         else (
           let cell_v = get_cell maze v in
           if Position.equal end_pos cell_v
           then Some (v :: path)
           else dfs (v :: path) cell_v))
  in
  match dfs [] start_pos with
  | None -> failwith "couldn't find path"
  | Some path -> Position.get_pos start_pos :: List.rev path
;;

let print_solution path maze =
  List.iter path ~f:(fun (row, col) -> maze.(row).(col) <- ' ');
  print_endline "solved maze:";
  Array.iter maze ~f:(fun row ->
    Array.to_list row |> String.of_list |> print_endline)
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let maze, start_pos, end_pos = Maze.of_file input_file in
        let path = solve_maze maze start_pos end_pos in
        print_solution path maze.maze]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
