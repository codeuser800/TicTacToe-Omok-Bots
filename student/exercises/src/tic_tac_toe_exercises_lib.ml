open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

(*Tries to see if there are nearby pieces to a position within a specific
  square*)
let rec pieces_nearby
  ~(pieces : Piece.t Position.Map.t)
  ~(game_kind : Game_kind.t)
  ~(num_steps : int)
  ~(direction : string)
  ~(curr_pos : Position.t)
  : bool
  =
  if num_steps = 0
  then false
  else if not (Position.in_bounds curr_pos ~game_kind)
  then false
  else (
    let piece_find = Map.find pieces curr_pos in
    match piece_find with
    | Some _piece_found -> true
    | None ->
      (match direction with
       | "ROW" ->
         pieces_nearby
           ~pieces
           ~game_kind
           ~num_steps:(num_steps - 1)
           ~direction
           ~curr_pos:(Position.right curr_pos)
       | "COL" ->
         (* print_string (string_of_int num_steps); print_string
            (Position.to_string curr_pos); *)
         pieces_nearby
           ~pieces
           ~game_kind
           ~num_steps:(num_steps - 1)
           ~direction
           ~curr_pos:(Position.down curr_pos)
       | "DIAG_RIGHT" ->
         pieces_nearby
           ~pieces
           ~game_kind
           ~num_steps:(num_steps - 1)
           ~direction
           ~curr_pos:(Position.down curr_pos |> Position.right)
       | "DIAG_LEFT" ->
         pieces_nearby
           ~pieces
           ~game_kind
           ~num_steps:(num_steps - 1)
           ~direction
           ~curr_pos:(Position.down curr_pos |> Position.left)
       | _ -> false))
;;

let pieces_within_square
  ~(pieces : Piece.t Position.Map.t)
  ~(game_kind : Game_kind.t)
  ~(num_steps : int)
  ~(curr_pos : Position.t)
  =
  pieces_nearby ~pieces ~game_kind ~num_steps ~direction:"ROW" ~curr_pos
  || pieces_nearby ~pieces ~game_kind ~num_steps ~direction:"COL" ~curr_pos
  || pieces_nearby
       ~pieces
       ~game_kind
       ~num_steps
       ~direction:"DIAG_RIGHT"
       ~curr_pos
  || pieces_nearby
       ~pieces
       ~game_kind
       ~num_steps
       ~direction:"DIAG_LEFT"
       ~curr_pos
;;

let is_neighbor
  ~(pieces : Piece.t Position.Map.t)
  ~(game_kind : Game_kind.t)
  ~(curr_pos : Position.t)
  =
  List.fold Position.all_offsets ~init:false ~f:(fun acc func ->
    if Position.in_bounds (func curr_pos) ~game_kind
    then (
      let potential_pieces = Map.find pieces (func curr_pos) in
      match potential_pieces with Some _piece -> true | None -> acc)
    else acc)
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let len = Game_kind.board_length game_kind in
  let board_1 =
    List.init len ~f:(fun row ->
      List.init len ~f:(fun col -> { Position.row; column = col }))
  in
  let board = List.concat board_1 in
  (* let set_board = Set.of_list (Position.t, Position.comparator) board
     in *)
  List.filter board ~f:(fun curr_piece ->
    is_neighbor ~pieces ~game_kind ~curr_pos:curr_piece)
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   For every position --> eval_helper for all 4 directions --> return true if
   1 of them is true

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

(*call this function for every single position in the board*)
let rec eval_helper
  ~(pieces : Piece.t Position.Map.t)
  ~(game_kind : Game_kind.t)
  ~(num_steps : int)
  ~(direction : string)
  ~(piece : Piece.t)
  ~(curr_pos : Position.t)
  : bool
  =
  if num_steps = 0
  then true
  else if not (Position.in_bounds curr_pos ~game_kind)
  then false
  else (
    let piece_find = Map.find pieces curr_pos in
    match piece_find with
    | None -> false
    | Some piece_found ->
      if not (Piece.equal piece_found piece)
      then false
      else (
        match direction with
        | "ROW" ->
          eval_helper
            ~pieces
            ~game_kind
            ~num_steps:(num_steps - 1)
            ~direction
            ~piece
            ~curr_pos:(Position.right curr_pos)
        | "COL" ->
          (* print_string (string_of_int num_steps); print_string
             (Position.to_string curr_pos); *)
          eval_helper
            ~pieces
            ~game_kind
            ~num_steps:(num_steps - 1)
            ~direction
            ~piece
            ~curr_pos:(Position.down curr_pos)
        | "DIAG_RIGHT" ->
          eval_helper
            ~pieces
            ~game_kind
            ~num_steps:(num_steps - 1)
            ~direction
            ~piece
            ~curr_pos:(Position.down curr_pos |> Position.right)
        | "DIAG_LEFT" ->
          eval_helper
            ~pieces
            ~game_kind
            ~num_steps:(num_steps - 1)
            ~direction
            ~piece
            ~curr_pos:(Position.down curr_pos |> Position.left)
        | _ -> false))
;;

let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  let board = Map.keys pieces in
  let win_len = Game_kind.win_length game_kind in
  let check_x_win =
    List.fold board ~init:Evaluation.Game_continues ~f:(fun acc position ->
      if eval_helper
           ~pieces
           ~game_kind
           ~num_steps:win_len
           ~direction:"ROW"
           ~piece:Piece.O
           ~curr_pos:position
         || eval_helper
              ~pieces
              ~game_kind
              ~num_steps:win_len
              ~direction:"COL"
              ~piece:Piece.O
              ~curr_pos:position
         || eval_helper
              ~pieces
              ~game_kind
              ~num_steps:win_len
              ~direction:"DIAG_RIGHT"
              ~piece:Piece.O
              ~curr_pos:position
         || eval_helper
              ~pieces
              ~game_kind
              ~num_steps:win_len
              ~direction:"DIAG_LEFT"
              ~piece:Piece.O
              ~curr_pos:position
      then Evaluation.Game_over { winner = Some Piece.O }
      else (
        match acc with
        | Evaluation.Game_over { winner = Some Piece.O } ->
          Evaluation.Game_over { winner = Some Piece.O }
        | Evaluation.Game_continues -> Evaluation.Game_continues
        | Evaluation.Illegal_state -> Evaluation.Illegal_state
        | Evaluation.Game_over _ -> Evaluation.Illegal_state))
  in
  List.fold board ~init:check_x_win ~f:(fun acc position ->
    if eval_helper
         ~pieces
         ~game_kind
         ~num_steps:win_len
         ~direction:"ROW"
         ~piece:Piece.X
         ~curr_pos:position
       || eval_helper
            ~pieces
            ~game_kind
            ~num_steps:win_len
            ~direction:"COL"
            ~piece:Piece.X
            ~curr_pos:position
       || eval_helper
            ~pieces
            ~game_kind
            ~num_steps:win_len
            ~direction:"DIAG_RIGHT"
            ~piece:Piece.X
            ~curr_pos:position
       || eval_helper
            ~pieces
            ~game_kind
            ~num_steps:win_len
            ~direction:"DIAG_LEFT"
            ~piece:Piece.X
            ~curr_pos:position
    then (
      match acc with
      | Evaluation.Game_over { winner = Some Piece.O } ->
        Evaluation.Illegal_state
      | Evaluation.Illegal_state -> Evaluation.Illegal_state
      | _ -> Evaluation.Game_over { winner = Some Piece.X })
    else (
      match acc with
      | Evaluation.Game_over { winner = Some Piece.X } ->
        Evaluation.Game_over { winner = Some Piece.X }
      | Evaluation.Game_over { winner = Some Piece.O } ->
        Evaluation.Game_over { winner = Some Piece.O }
      | Evaluation.Game_continues -> Evaluation.Game_continues
      | Evaluation.Illegal_state -> Evaluation.Illegal_state
      | _ -> Evaluation.Game_continues))
;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let poss_moves = available_moves ~game_kind ~pieces in
  List.filter poss_moves ~f:(fun move ->
    let potential_win =
      evaluate ~game_kind ~pieces:(Map.set pieces ~key:move ~data:me)
    in
    match potential_win with
    | Evaluation.Game_over { winner = Some pot_winner } ->
      if Piece.equal pot_winner me then true else false
    | _ -> false)
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  winning_moves ~me:(Piece.flip me) ~game_kind ~pieces
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    OOX
    OXO
    XOO |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
let%expect_test "yes available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect
    {|
   (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
    ((row 1) (column 2)) ((row 2) (column 1))) |}]
;;

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)

let%expect_test "evalulate_win_for_x" =
  print_endline
    (evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evalulate_non_win" =
  print_endline
    (evaluate ~game_kind:non_win.game_kind ~pieces:non_win.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" =
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| ((((row 1) (column 1))))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect
    {|
  ((((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2)) ((row 2)
  (column 1)))) |}]
;;
