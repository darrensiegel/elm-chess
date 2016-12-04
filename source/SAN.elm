module SAN exposing (toHalfMove, HalfMove, AllMoves)

import Core exposing (..)
import Move as Move exposing (..)
import String


type alias HalfMove =
    String


type alias AllMoves =
    List HalfMove


stripMaybe : Maybe GameSquare -> GameSquare
stripMaybe maybeGameSquare =
    case maybeGameSquare of
        Just value ->
            value

        Nothing ->
            Vacant


abbrev : Piece -> String
abbrev piece =
    case piece of
        Pawn ->
            ""

        Rook ->
            "R"

        Knight ->
            "N"

        Bishop ->
            "B"

        Queen ->
            "Q"

        King ->
            "K"


checkSuffix : Player -> GameModel -> String
checkSuffix player model =
    let
        inCheck =
            Move.kingUnderAttack (Move.opponent player) model.board

        noMoves =
            (List.length (Move.allAvailableMoves (Move.opponent player) model)) == 0
    in
        if (noMoves) then
            "#"
        else if (inCheck) then
            "+"
        else
            ""


moveDesc : Player -> Move -> Piece -> GameSquare -> GameModel -> String
moveDesc player ( src, dest ) srcPiece destSquare model =
    let
        xDiff =
            (dest.x - src.x)

        castle =
            case destSquare of
                Vacant ->
                    if (srcPiece == King && (abs (dest.x - src.x) > 1)) then
                        True
                    else
                        False

                _ ->
                    False

        capture =
            case destSquare of
                Occupied _ _ ->
                    True

                _ ->
                    False
    in
        if (castle && xDiff == 2) then
            "O-O"
        else if (castle && xDiff == -2) then
            "O-O-O"
        else if (capture) then
            case srcPiece of
                Pawn ->
                    (file src) ++ "x" ++ (toAlgebraic dest)

                _ ->
                    (abbrev srcPiece) ++ (maybeFile player src dest srcPiece model) ++ "x" ++ (toAlgebraic dest)
        else
            (abbrev srcPiece) ++ (maybeFile player src dest srcPiece model) ++ (toAlgebraic dest)


file : Position -> String
file position =
    String.left 1 (toAlgebraic position)


maybeFile : Player -> Position -> Position -> Piece -> GameModel -> String
maybeFile player src dest piece model =
    let
        ambiguous =
            isAmbiguous player src dest piece model
    in
        if (ambiguous) then
            file src
        else
            ""


toHalfMove : Move -> GameModel -> HalfMove
toHalfMove ( src, dest ) model =
    let
        playerThatMoved =
            if model.whitesMove then
                White
            else
                Black

        srcSquare =
            Move.getGameSquare src model.board |> stripMaybe

        destSquare =
            Move.getGameSquare dest model.board |> stripMaybe

        updatedBoard =
            Move.applyMove src dest model.board
    in
        case srcSquare of
            Occupied player piece ->
                (moveDesc player ( src, dest ) piece destSquare model) ++ (checkSuffix playerThatMoved { model | board = updatedBoard })

            Vacant ->
                ""


samePiece : Piece -> Maybe GameSquare -> Bool
samePiece srcPiece gameSquare =
    case gameSquare of
        Just value ->
            case value of
                Occupied _ piece ->
                    piece == srcPiece

                Vacant ->
                    False

        Nothing ->
            False


isAmbiguous : Player -> Position -> Position -> Piece -> GameModel -> Bool
isAmbiguous player src dest srcPiece model =
    let
        length =
            Move.allAvailableMoves player model
                |> List.filter (\( s, d ) -> d == dest)
                |> List.map (\( s, d ) -> Move.getGameSquare s model.board)
                |> List.filter (samePiece srcPiece)
                |> List.length
    in
        length > 1
