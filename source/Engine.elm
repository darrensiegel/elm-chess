module Engine exposing (nextMove)

import Array exposing (Array)
import Core exposing (..)
import Eval as Eval exposing (..)
import Find exposing (..)
import Move as Move exposing (..)
import Task exposing (..)


nextMove : GameModel -> Task x (Maybe Move)
nextMove gameModel =
    let
        ( score, move ) =
            List.map (\m -> ( applyMove m gameModel, m )) (nextMoves gameModel)
                |> List.foldr executeMin ( -1000.0, dummyMove )
    in
    if move == dummyMove then
        Task.succeed Nothing

    else
        Task.succeed (Just move)


executeMin : ( GameModel, Move ) -> ( Float, Move ) -> ( Float, Move )
executeMin ( model, move ) ( bestScore, bestMove ) =
    let
        newBeta =
            abMin 1 bestScore 1000 model
    in
    if newBeta > bestScore then
        ( newBeta, move )

    else
        ( bestScore, bestMove )


dummyMove =
    ( Position 0 0, Position 0 0 )


applyMove : Move -> GameModel -> GameModel
applyMove move model =
    Move.updateGameModel move model


logSize : Moves -> Moves
logSize moves =
    let
        _ =
            Debug.log "MOVE COUNT: " (String.fromInt (List.length moves))
    in
    moves


type alias MoveToCompare =
    { src : Position
    , dest : Position
    , srcSquare : Maybe GameSquare
    , destSquare : Maybe GameSquare
    }


moveComparison : MoveToCompare -> MoveToCompare -> Order
moveComparison a b =
    let
        aDestSquare =
            Maybe.withDefault Vacant a.destSquare

        bDestSquare =
            Maybe.withDefault Vacant b.destSquare
    in
    case aDestSquare of
        Vacant ->
            case bDestSquare of
                Vacant ->
                    EQ

                _ ->
                    LT

        _ ->
            case bDestSquare of
                Vacant ->
                    GT

                _ ->
                    EQ


sortByAttacks : Player -> GameModel -> Moves -> Moves
sortByAttacks player model moves =
    let
        targetSquares =
            List.map (\( src, dest ) -> MoveToCompare src dest (Move.getGameSquare src model.board) (Move.getGameSquare dest model.board)) moves
    in
    List.sortWith moveComparison targetSquares
        |> List.map (\mv -> ( mv.src, mv.dest ))


nextMoves : GameModel -> Moves
nextMoves model =
    let
        player =
            if model.whitesMove then
                White

            else
                Black
    in
    Move.allAvailableMoves player model |> sortByAttacks player model


stripMaybe : Maybe Move -> Move
stripMaybe list =
    case list of
        Just value ->
            value

        Nothing ->
            dummyMove


stripMaybeList : Maybe Moves -> Moves
stripMaybeList list =
    case list of
        Just value ->
            value

        Nothing ->
            []


abMinHelper : Int -> Float -> Float -> GameModel -> Moves -> Float
abMinHelper depth alpha beta model moves =
    let
        listLength =
            List.length moves

        move =
            List.head moves |> stripMaybe

        newV =
            if listLength > 0 then
                abMax (depth - 1) alpha beta (applyMove move model)

            else
                beta

        newB =
            min beta newV
    in
    if listLength == 0 then
        beta

    else if newB <= alpha then
        alpha

    else
        abMinHelper depth alpha newB model (List.tail moves |> stripMaybeList)


abMin : Int -> Float -> Float -> GameModel -> Float
abMin depth alpha beta model =
    if depth == 0 then
        Eval.eval model

    else
        abMinHelper depth alpha beta model (nextMoves model)


abMaxHelper : Int -> Float -> Float -> GameModel -> Moves -> Float
abMaxHelper depth alpha beta model moves =
    let
        listLength =
            List.length moves

        move =
            List.head moves |> stripMaybe

        newV =
            if listLength > 0 then
                abMin (depth - 1) alpha beta (applyMove move model)

            else
                alpha

        newA =
            max alpha newV
    in
    if listLength == 0 then
        alpha

    else if beta <= newA then
        beta

    else
        abMaxHelper depth newA beta model (List.tail moves |> stripMaybeList)


abMax : Int -> Float -> Float -> GameModel -> Float
abMax depth alpha beta model =
    if depth == 0 then
        Eval.eval model

    else
        abMaxHelper depth alpha beta model (nextMoves model)
