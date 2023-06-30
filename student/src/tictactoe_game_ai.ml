open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  List.random_element_exn
    (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces)
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let win_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  match win_moves with
  | [] -> random_move_strategy ~game_kind ~pieces
  | _ -> List.random_element_exn win_moves
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let block_moves =
    Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
  in
  match block_moves with
  | [] -> pick_winning_move_if_possible_strategy ~me ~game_kind ~pieces
  | _ -> List.random_element_exn block_moves
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_or_block_if_possible_strategy

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let score = Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces in
  (* let () = Core.print_s [%message (pieces : Piece.t Position.Map.t) (me :
     Piece.t)] in *)
  match score with
  | Game_over { winner = Some someone } ->
    if Piece.equal someone me then Float.infinity else Float.neg_infinity
  | _ -> 0.0
;;

let _ = score

let rec minimax
  ~(game_kind : Game_kind.t)
  ~(me : Piece.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(depth : int)
  ~(maximizing : bool)
  =
  let available_moves =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  (* let () = Core.print_s [%message (depth : int)] in *)
  (* let () = Core.print_s [%message (available_moves : Position.t list)] in *)
  (* let () = Async.print_s [%message (maximizing : bool)] in *)
  let score_curr = score ~me ~game_kind ~pieces in
  (* let () = Async.print_s [%message (score_curr : float)] in *)
  if depth = 0
     || List.length available_moves = 1
     || Float.( = ) score_curr Float.neg_infinity
     || Float.( = ) score_curr Float.infinity
  then (* let () = Core.print_s [%message "here"] in *)
    score_curr
  else if maximizing
  then (
    let max_elt =
      List.max_elt
        (List.map available_moves ~f:(fun move ->
           minimax
             ~game_kind
             ~me
             ~pieces:(Map.set pieces ~key:move ~data:me)
             ~depth:(depth - 1)
             ~maximizing:(not maximizing)))
        ~compare:[%compare: float]
      |> Option.value ~default:Float.neg_infinity
    in
    max_elt
    (* List.fold available_moves ~init:Float.neg_infinity ~f:(fun value move
       -> let new_value = minimax ~game_kind ~me ~pieces:(Map.set pieces
       ~key:move ~data:me) ~depth:(depth - 1) ~maximizing:(not maximizing) in
       if Float.(new_value > value) then new_value else value) *))
  else (
    let min_elt =
      List.min_elt
        (List.map available_moves ~f:(fun move ->
           minimax
             ~game_kind
             ~me
             ~pieces:(Map.set pieces ~key:move ~data:(Piece.flip me))
             ~depth:(depth - 1)
             ~maximizing:(not maximizing)))
        ~compare:[%compare: float]
      |> Option.value ~default:Float.infinity
    in
    min_elt)
;;

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf. [compute_next_move] is only called whenever it is your turn, the
   game isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  (* let () = Core.print_s [%message "available moves"] in let
     available_moves = Tic_tac_toe_exercises_lib.available_moves
     ~game_kind:game_state.game_kind ~pieces:game_state.pieces in let () =
     print_string "here winning" in let pot_winning_moves =
     Tic_tac_toe_exercises_lib.winning_moves ~me
     ~game_kind:game_state.game_kind ~pieces:game_state.pieces in let
     losing_moves = Tic_tac_toe_exercises_lib.losing_moves ~me
     ~game_kind:game_state.game_kind ~pieces:game_state.pieces in if not
     (List.is_empty pot_winning_moves) then List.random_element_exn
     pot_winning_moves else if not (List.is_empty losing_moves) then
     List.random_element_exn losing_moves else ( *)
  let available_moves =
    Tic_tac_toe_exercises_lib.available_moves
      ~game_kind:game_state.game_kind
      ~pieces:game_state.pieces
  in
  (* let () = Core.print_s [%message "available moves"] in *)
  let first_move = List.random_element_exn available_moves in
  let score, curr_pos =
    ( minimax
        ~game_kind:game_state.game_kind
        ~me
        ~pieces:(Map.set game_state.pieces ~key:first_move ~data:me)
        ~depth:1
        ~maximizing:false
    , first_move )
  in
  let _final_score, final_pos =
    List.fold
      available_moves
      ~init:(score, curr_pos)
      ~f:(fun (best_score, best_move) potential_move ->
      let curr_score =
        minimax
          ~game_kind:game_state.game_kind
          ~me
          ~pieces:(Map.set game_state.pieces ~key:potential_move ~data:me)
          ~depth:1
          ~maximizing:false
      in
      (* let () = Core.print_s [%message (curr_score : float) (potential_move
         : Position.t)] in *)
      if Float.( > ) curr_score best_score
      then curr_score, potential_move
      else best_score, best_move)
  in
  (* let () = Async.print_s [%message (final_score : float) (final_pos :
     Position.t)] in *)
  final_pos
;;
