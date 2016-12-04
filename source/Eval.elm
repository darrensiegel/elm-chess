module Eval exposing (eval)

import Find exposing (..)
import Core exposing (..)
import Move as Move exposing (..)
import Array exposing (Array)
import Task exposing (..)
import Set exposing (..)


type alias File =
    List ( Position, GameSquare )


type alias EvaluatorFunc =
    Player -> GameModel -> Moves -> Float


type alias Evaluator =
    ( EvaluatorFunc, Float )



-- An extensible evaluation model


pieceFactor =
    1


evaluators =
    [ ( pieceDiff King, (500.0 * pieceFactor) )
    , ( pieceDiff Queen, (9.0 * pieceFactor) )
    , ( pieceDiff Rook, (5.0 * pieceFactor) )
    , ( pieceDiff Bishop, (3.0 * pieceFactor) )
    , ( pieceDiff Knight, (3.0 * pieceFactor) )
    , ( pieceDiff Pawn, (1.0 * pieceFactor) )
    , ( mobility, 0.1 )
    , ( space, 0.1 )
    , ( doubledPawns, -0.5 )
    , ( isolatedPawns, -0.5 )
    , ( center, 0.5 )
    , ( threats, 0.5 )
    ]


eval : GameModel -> Float
eval model =
    performEval model


execute : Player -> Player -> GameModel -> Moves -> Moves -> Evaluator -> Float
execute player1 player2 model player1Moves player2Moves ( evaluatorFunc, weight ) =
    weight * ((evaluatorFunc player1 model player1Moves) - (evaluatorFunc player2 model player2Moves))


performEval : GameModel -> Float
performEval model =
    let
        player1 =
            Black

        player2 =
            White

        availableMoves =
            Move.allAvailableMoves player1 model

        opponentMoves =
            Move.allAvailableMoves player2 model
    in
        List.map (execute player1 player2 model availableMoves opponentMoves) evaluators
            |> List.foldr (+) 0.0


centerPredicate : Player -> ( Position, GameSquare ) -> Bool
centerPredicate player tuple =
    let
        ( position, gameSquare ) =
            tuple
    in
        case gameSquare of
            Occupied player _ ->
                (position.x == 3 || position.x == 4) && (position.y == 3 || position.y == 4)

            _ ->
                False


center : Player -> GameModel -> Moves -> Float
center player model moves =
    let
        playerInCenter =
            (findPiecesBy (centerPredicate player) model.board) |> List.length
    in
        toFloat playerInCenter


occupiedBy : Player -> Board -> Position -> Maybe Position
occupiedBy player board position =
    let
        maybeGameSquare =
            Move.getGameSquare position board
    in
        case maybeGameSquare of
            Just value ->
                case value of
                    Occupied p _ ->
                        if (p == player) then
                            Just position
                        else
                            Nothing

                    _ ->
                        Nothing

            Nothing ->
                Nothing


threats : Player -> GameModel -> Moves -> Float
threats player model moves =
    List.map (\( src, dest ) -> dest) moves
        |> List.filterMap (occupiedBy (opponent player) model.board)
        |> List.length
        |> toFloat


yComparison a b =
    let
        ( posA, sqA ) =
            a

        ( posB, sqB ) =
            b
    in
        compare posA.y posB.y


file : GameModel -> Int -> File
file model index =
    findPiecesBy (\( position, gameSquare ) -> position.x == index) model.board
        |> List.sortWith yComparison


adjacentFiles : GameModel -> Int -> ( File, File )
adjacentFiles model fileIndex =
    ( file model (fileIndex - 1), file model (fileIndex + 1) )


backwardPawns : Player -> GameModel -> Int
backwardPawns player model =
    0


onlyPawns : Player -> ( Position, GameSquare ) -> Bool
onlyPawns player tuple =
    let
        ( _, gameSquare ) =
            tuple
    in
        case gameSquare of
            Occupied player Pawn ->
                True

            _ ->
                False


hasDoubledPawn : Int -> GameModel -> Player -> ( Position, GameSquare ) -> Maybe ( Position, GameSquare )
hasDoubledPawn direction model player tuple =
    let
        ( position, _ ) =
            tuple
    in
        case (getGameSquare (Position position.x (position.y + direction)) model.board) of
            Just value ->
                case value of
                    Occupied player Pawn ->
                        Just tuple

                    _ ->
                        Nothing

            Nothing ->
                Nothing


doubledPawns : Player -> GameModel -> Moves -> Float
doubledPawns player model moves =
    let
        direction =
            if player == White then
                -1
            else
                1
    in
        findPiecesBy (byPlayerPiecePredicate player Pawn) model.board
            |> List.filterMap (hasDoubledPawn direction model player)
            |> List.length
            |> toFloat


toSetHelper : List ( Position, GameSquare ) -> Set Int -> Set Int
toSetHelper list set =
    let
        hd =
            List.head list
    in
        case hd of
            Just tuple ->
                Set.insert (Tuple.first tuple).x set

            Nothing ->
                set


toSet : List ( Position, GameSquare ) -> Set Int
toSet list =
    toSetHelper list Set.empty


filterForPawns player piece ( position, gameSquare ) =
    case gameSquare of
        Occupied player piece ->
            True

        _ ->
            False


countInFile : File -> Piece -> Player -> Int
countInFile file piece player =
    List.filter (filterForPawns player piece) file
        |> List.length


countIsolated : Player -> ( File, File ) -> Int -> Int
countIsolated player ( file1, file2 ) accumulator =
    let
        count1 =
            countInFile file1 Pawn player

        count2 =
            countInFile file2 Pawn player
    in
        if (count1 + count2 == 0) then
            accumulator + 1
        else
            accumulator


isolatedPawns : Player -> GameModel -> Moves -> Float
isolatedPawns player model moves =
    let
        filesWithPawns =
            findPiecesBy (byPlayerPiecePredicate player Pawn) model.board
                |> toSet
                |> Set.toList
    in
        List.map (\index -> adjacentFiles model index) filesWithPawns
            |> List.foldr (countIsolated player) 0
            |> toFloat



-- A count of how many positions a player can attack on the
-- opponents side


space : Player -> GameModel -> Moves -> Float
space player model moves =
    let
        offset =
            if player == Black then
                0
            else
                4
    in
        List.filter (\( src, dest ) -> dest.y < (8 - offset) && dest.y >= (4 - offset)) moves
            |> List.length
            |> toFloat


filterCheckConstrained : Player -> Position -> GameModel -> List Position -> List Position
filterCheckConstrained player src model availableMoves =
    List.filter (\dest -> not (Move.moveIntroducesCheck src dest player model)) availableMoves


allMovesFromPosition : Player -> GameModel -> ( Position, GameSquare ) -> Moves
allMovesFromPosition player model tuple =
    let
        ( position, gameSquare ) =
            tuple

        pieces =
            findPiecesBy (piecesByPlayerPredicate player) model.board
    in
        List.map (validMovesFromTuple model) pieces
            |> List.concat
            |> (filterCheckConstrained player position model)
            |> List.map (\dest -> ( position, dest ))



-- Turn into a List of tuples


allMoves : Player -> GameModel -> Moves
allMoves player model =
    let
        pieces =
            findPiecesBy (byPlayerPredicate player) model.board
    in
        List.map (allMovesFromPosition player model) pieces
            |> List.concat


countMobile : Player -> ( Position, GameSquare ) -> Int -> Int
countMobile player tuple accumulator =
    let
        ( position, gameSquare ) =
            tuple

        rank =
            if player == White then
                7
            else
                0
    in
        case gameSquare of
            Occupied _ Rook ->
                if (position == (Position 0 rank)) || (position == (Position 7 rank)) then
                    accumulator
                else
                    accumulator + 1

            Occupied _ Bishop ->
                if (position == (Position 2 rank)) || (position == (Position 5 rank)) then
                    accumulator
                else
                    accumulator + 1

            Occupied _ Queen ->
                if (position == (Position 3 rank)) then
                    accumulator
                else
                    accumulator + 1

            Occupied _ Knight ->
                if (position == (Position 1 rank)) || (position == (Position 6 rank)) then
                    accumulator
                else
                    accumulator + 1

            _ ->
                accumulator



-- Return the number of pices (not counting pawns) that are
-- not in their original starting positions and thus 'mobilized'
-- to attack


mobility : Player -> GameModel -> Moves -> Float
mobility player model _ =
    findPiecesBy (byPlayerPredicate player) model.board
        |> (List.foldr (countMobile player) 0)
        |> toFloat


pieceDiff : Piece -> Player -> GameModel -> Moves -> Float
pieceDiff piece player model moves =
    toFloat (List.length <| findPiecesBy (byPlayerPiecePredicate player piece) model.board)
