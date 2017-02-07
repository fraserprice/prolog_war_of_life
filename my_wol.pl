:- use_module(library(system)).

% Plays NumGames games with blue playing strategy F and red playing strategy S. Prints statistics for games
test_strategy(NumGames, F, S) :-
    datime(DateTime),
    datime(StartStamp, DateTime),
    test_strategy_aux(NumGames, F, S, 0, 0, 0, -1, 100000, 0, 0, StartStamp).
    
test_strategy_aux(0, _, _, Draws, BWins, RWins, Long, Short, MoveSum, GP, StartStamp) :-
    datime(DateTime),
    datime(NowStamp, DateTime),
    AverageTime is (NowStamp - StartStamp) / GP,
    AverageMoves is MoveSum / GP,
    format('Draws: ~w~nBWins: ~w~nRWins: ~w~nLongest: ~w~nShortest: ~w~nAverage Moves: ~w~nAverage Time: ~w~n', 
        [Draws, BWins, RWins, Long, Short, AverageMoves, AverageTime]).
test_strategy_aux(GamesRemaining, F, S, Draws, BWins, RWins, Long, Short, MoveSum, GP, StartStamp) :-
    GamesRemaining > 0,
    play(quiet, F, S, Moves, Winner),
    (Winner == b -> (BWin = 1, RWin = 0, Draw = 0) ; 
    (Winner == r -> (BWin = 0, RWin = 1, Draw = 0) ; 
    BWin = 0, RWin = 0, Draw = 1)),
    NewGamesRemaining is GamesRemaining - 1,
    NewDraws is Draws + Draw,
    NewBWins is BWins + BWin,
    NewRWins is RWins + RWin,
    NewLong is max(Long, Moves),
    NewShort is min(Short, Moves),
    NewMoveSum is MoveSum + Moves,
    NewGP is GP + 1,
    test_strategy_aux(NewGamesRemaining, F, S, NewDraws, NewBWins, 
        NewRWins, NewLong, NewShort, NewMoveSum, NewGP, StartStamp).

% Get set of possible moves for given piece and board state
get_piece_possible_moves([B, R], [X, Y], PossibleMoves) :-
    findall([X, Y, NX, NY], 
    (
        neighbour_position(X, Y, [NX, NY]),
        (member([X, Y], B) ; member([X, Y], R)),
        \+ member([NX, NY], B),
        \+ member([NX, NY], R)
    ), PossibleMoves).
    
% Get set of all possible moves for a given colour and board state
get_all_possible_moves(PlayerColour, Board, Moves) :-
    get_all_possible_moves_aux(PlayerColour, Board, Board, Moves).
get_all_possible_moves_aux(b, [[], _], _, []).
get_all_possible_moves_aux(r, [_, []], _, []).
get_all_possible_moves_aux(b, [[P | T], R], FullBoard, PossibleMoves) :-
    get_piece_possible_moves(FullBoard, P, PPossibleMoves),
    get_all_possible_moves_aux(b, [T, R], FullBoard, NextPossibleMoves),
    append(PPossibleMoves, NextPossibleMoves, PossibleMoves).
get_all_possible_moves_aux(r, [B, [P | T]], FullBoard, PossibleMoves) :-
    get_piece_possible_moves(FullBoard, P, PPossibleMoves),
    get_all_possible_moves_aux(r, [B, T], FullBoard, NextPossibleMoves),
    append(PPossibleMoves, NextPossibleMoves, PossibleMoves).

% Applies move to given board and produces new board. Note that this function assumes move is valid
apply_move(b, [X, Y, NX, NY], [B, R], NewBoard) :-
    delete(B, [X, Y], DelB),
    append([[NX, NY]], DelB, NewB),
    NewBoard = [NewB, R].
apply_move(r, [X, Y, NX, NY], [B, R], NewBoard) :-
    delete(R, [X, Y], DelR),
    append([[NX, NY]], DelR, NewR),
    NewBoard = [B, NewR].

% Produces a list of pairs which contain all valid moves + board states after move + c's crank is applied
% to given board for given colour
get_valid_moves_and_new_crank_states(PlayerColour, CurrentBoardState, ValidMovesCranks) :-
    get_all_possible_moves(PlayerColour, CurrentBoardState, PossibleMoves),
    findall([ValidMove, BoardWithMoveAndCrank], 
    (
        member(ValidMove, PossibleMoves),
        apply_move(PlayerColour, ValidMove, CurrentBoardState, PreCrankState),
        next_generation(PreCrankState, BoardWithMoveAndCrank)
    ), ValidMovesCranks).

% Compares two boards for given colour; yes if board 1 is better than board 2 for bloodlust
bloodlust_compare_boards(PlayerColour, [B1, R1], [B2, R2]) :-
    PlayerColour == b ->
    (length(R1, LR1), length(R2, LR2), LR1 < LR2);
    (length(B1, LB1), length(B2, LB2), LB1 < LB2).

% Compares two boards for given colour; yes if board 1 is better than board 2 for self_preservation
sp_compare_boards(PlayerColour, [B1, R1], [B2, R2]) :-
    PlayerColour == b ->
    (length(B1, LB1), length(B2, LB2), LB1 > LB2);
    (length(R1, LR1), length(R2, LR2), LR1 > LR2).

% Compares two boards for given colour; yes if board 1 is better than board 2 for land_grab
lg_compare_boards(PlayerColour, [B1, R1], [B2, R2]) :-
    PlayerColour == b ->
    (length(B1, LB1), length(B2, LB2),
    length(R1, LR1), length(R2, LR2),
    LB1 - LR1 >= LB2 - LR2);
    (length(B1, LB1), length(B2, LB2),
    length(R1, LR1), length(R2, LR2),
    LR1 - LB1 >= LR2 - LB2).

% Computes best move for given colour, given a board state + strategy
best_move(_, _, [[BestMove, [_, _]] | []], BestMove).
best_move(b, Strategy, [[Move1, [B1, R1]], [_, [B2, R2]] | T], BestMove) :-
    (
        Strategy == bloodlust, bloodlust_compare_boards(b, [B1, R1], [B2, R2]) ;
        Strategy == self_preservation, sp_compare_boards(b, [B1, R1], [B2, R2]) ;
        Strategy == land_grab, lg_compare_boards(b, [B1, R1], [B2, R2])
    ),
    best_move(b, Strategy, [[Move1, [B1, R1]] | T], BestMove).
best_move(b, Strategy, [[_, [B1, R1]], [Move2, [B2, R2]] | T], BestMove) :-
    (
        Strategy == bloodlust, \+ bloodlust_compare_boards(b, [B1, R1], [B2, R2]) ;
        Strategy == self_preservation, \+ sp_compare_boards(b, [B1, R1], [B2, R2]) ;
        Strategy == land_grab, \+ lg_compare_boards(b, [B1, R1], [B2, R2])
    ),
    best_move(b, Strategy, [[Move2, [B2, R2]] | T], BestMove).
best_move(r, Strategy, [[Move1, [B1, R1]], [_, [B2, R2]] | T], BestMove) :-
    (
        Strategy == bloodlust, bloodlust_compare_boards(r, [B1, R1], [B2, R2]) ;
        Strategy == self_preservation, sp_compare_boards(r, [B1, R1], [B2, R2]) ;
        Strategy == land_grab, lg_compare_boards(r, [B1, R1], [B2, R2])
    ),
    best_move(r, Strategy, [[Move1, [B1, R1]] | T], BestMove).
best_move(r, Strategy, [[_, [B1, R1]], [Move2, [B2, R2]] | T], BestMove) :-
    (
        Strategy == bloodlust, \+ bloodlust_compare_boards(r, [B1, R1], [B2, R2]) ;
        Strategy == self_preservation, \+ sp_compare_boards(r, [B1, R1], [B2, R2]) ;
        Strategy == land_grab, \+ lg_compare_boards(r, [B1, R1], [B2, R2])
    ),
    best_move(r, Strategy, [[Move2, [B2, R2]] | T], BestMove).

% Computes and applies best move to given board state for given colour and strategy
apply_best_move(PlayerColour, Strategy, CurrentBoardState, NewBoardState, Move) :-
    get_valid_moves_and_new_crank_states(PlayerColour, CurrentBoardState, ValidMovesCranks),
    best_move(PlayerColour, Strategy, ValidMovesCranks, Move),
    apply_move(PlayerColour, Move, CurrentBoardState, NewBoardState).

bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :- 
    apply_best_move(PlayerColour, bloodlust, CurrentBoardState, NewBoardState, Move).

self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move) :- 
    apply_best_move(PlayerColour, self_preservation, CurrentBoardState, NewBoardState, Move).

land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
    apply_best_move(PlayerColour, land_grab, CurrentBoardState, NewBoardState, Move).

minimax(PlayerColour, CurrentBoardState, NewBoardState, Move) :- 
    (PlayerColour == r -> OpponentColour = b ; OpponentColour = r),
    get_valid_moves_and_new_crank_states(PlayerColour, CurrentBoardState, ValidMovesCranks),
    minimax_aux(PlayerColour, OpponentColour, [100, []], ValidMovesCranks, Move),
    apply_move(PlayerColour, Move, CurrentBoardState, NewBoardState).

% For each possible player move, we compute best opponent move, and pick the player move which corresponds to
% opponent's worst best move as minimax dictates
minimax_aux(_, _, [_, BestMove], [], BestMove).
minimax_aux(PlayerColour, OpponentColour, [CurrBestScore, CurrBestMove], [[Move, Board] | T], BestMove) :-
    (
        apply_best_move(OpponentColour, land_grab, Board, PreCrankState, _) ;
        PreCrankState = Board
    ),
    next_generation(PreCrankState, NewBoardState),
    minimax_score_board(OpponentColour, NewBoardState, Score),
    (
        (Score < CurrBestScore,
        minimax_aux(PlayerColour, OpponentColour, [Score, Move], T, BestMove)) ;
        (Score >= CurrBestScore, 
        minimax_aux(PlayerColour, OpponentColour, [CurrBestScore, CurrBestMove], T, BestMove))
    ).

% Scores board based on land_grab scoring
minimax_score_board(b, [B, R], Score) :-
    length(B, BL), length(R, RL),
    Score is BL - RL.
minimax_score_board(r, [B, R], Score) :-
    length(B, BL), length(R, RL),
    Score is RL - BL.

