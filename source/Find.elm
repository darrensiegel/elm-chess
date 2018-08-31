module Find exposing (PiecePredicate, allPiecesPredicate, byPlayerPiecePredicate, byPlayerPredicate, byPositionPredicate, entireBoardPredicate, findPiecesBy, kingSidePredicate, piecesByPlayerPredicate, queenSidePredicate, toFlatIndexedList, toPositionTuple)

-- Module containing functions for finding (i.e. selectng) pieces from
-- the board

import Array exposing (Array)
import Char exposing (..)
import Core exposing (..)


type alias PiecePredicate =
    ( Position, GameSquare ) -> Bool



-- Some useful piece predicates


entireBoardPredicate : ( Position, GameSquare ) -> Bool
entireBoardPredicate tuple =
    True


allPiecesPredicate : ( Position, GameSquare ) -> Bool
allPiecesPredicate tuple =
    let
        ( position, gameSquare ) =
            tuple
    in
    case gameSquare of
        Vacant ->
            False

        _ ->
            True


piecesByPlayerPredicate : Player -> ( Position, GameSquare ) -> Bool
piecesByPlayerPredicate player tuple =
    let
        ( position, gameSquare ) =
            tuple
    in
    case gameSquare of
        Vacant ->
            False

        --  Occupied plyr piece -> player == plyr
        Occupied ply piece ->
            if player == ply then
                True

            else
                False


byPositionPredicate : Position -> ( Position, GameSquare ) -> Bool
byPositionPredicate position tuple =
    let
        ( pos, _ ) =
            tuple
    in
    pos == position


byPlayerPiecePredicate : Player -> Piece -> ( Position, GameSquare ) -> Bool
byPlayerPiecePredicate player piece tuple =
    let
        ( _, gameSquare ) =
            tuple
    in
    case gameSquare of
        Vacant ->
            False

        Occupied plyr pce ->
            (player == plyr) && (piece == pce)


byPlayerPredicate : Player -> ( Position, GameSquare ) -> Bool
byPlayerPredicate player tuple =
    let
        ( _, gameSquare ) =
            tuple
    in
    case gameSquare of
        Vacant ->
            False

        Occupied plyr _ ->
            player == plyr


queenSidePredicate : Position -> ( Position, GameSquare ) -> Bool
queenSidePredicate position tuple =
    let
        ( src, _ ) =
            tuple
    in
    (position.y == src.y) && ((position.x - 1 == src.x) || (position.x - 2 == src.x))


kingSidePredicate : Position -> ( Position, GameSquare ) -> Bool
kingSidePredicate position tuple =
    let
        ( src, _ ) =
            tuple
    in
    (position.y == src.y) && ((position.x + 1 == src.x) || (position.x + 2 == src.x))


toFlatIndexedList : Board -> List ( Int, GameSquare )
toFlatIndexedList board =
    List.indexedMap (\i n -> ( i, n )) (List.concat (List.map Array.toList (Array.toList board)))


toPositionTuple : ( Int, GameSquare ) -> ( Position, GameSquare )
toPositionTuple tuple =
    let
        ( index, gameSquare ) =
            tuple
    in
    ( Position (remainderBy 8 (index + 8)) (index // 8), gameSquare )


findPiecesBy : PiecePredicate -> Board -> List ( Position, GameSquare )
findPiecesBy predicate board =
    let
        listOfTuples =
            toFlatIndexedList board |> List.map toPositionTuple
    in
    List.filter predicate listOfTuples
