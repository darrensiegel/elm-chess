module Move exposing (MoveFunc, allAttackFree, allAvailableMoves, applyMove, bishopMovements, bishopPositions, checkEnPassant, checkForAttacks, dropMaybe, dumpItem, dumpList, emptyPosition, facesAttackFrom, fromAlgebraic, getGameSquare, getGameSquareByRow, getGameSquareMaybe, handleCastling, handleEnPassantCapture, handlePawnPromotions, identity, isOpponentPiece, isVacant, kingMoves, kingPositions, kingUnderAttack, knightMoves, knightPositions, lazilyWalkDirection, logList, maybeAppend, moveE, moveIntroducesCheck, moveN, moveNE, moveNW, moveS, moveSE, moveSW, moveW, occupiedByPlayer, opponent, pawnPositionHelper, pawnPositions, posToString, processKingCastle, processQueenCastle, queenMovements, queenPositions, removeNothing, rookMovements, rookPositions, setAdjacentSquare, setGameSquare, setVacant, toAlgebraic, toggleWhitesMove, updateBlackCastle, updateBoard, updateEnPassant, updateFullMove, updateGameModel, updateHalfMove, updateWhiteCastle, updateWhitesMove, validMoves, validMovesFromTuple, validMovesPerPiece, validateAttackPosition, validateEmptyPosition, validatePosition, validatePositionRow, walk, walkDirection)

import Array exposing (Array)
import Char exposing (..)
import Core exposing (..)
import Find exposing (..)


type alias MoveFunc =
    Position -> Position



-- Our move functions


moveN position =
    Position position.x (position.y - 1)


moveS position =
    Position position.x (position.y + 1)


moveE position =
    Position (position.x + 1) (position.y - 0)


moveW position =
    Position (position.x - 1) (position.y - 0)


moveNW position =
    Position (position.x - 1) (position.y - 1)


moveNE position =
    Position (position.x + 1) (position.y - 1)


moveSE position =
    Position (position.x + 1) (position.y + 1)


moveSW position =
    Position (position.x - 1) (position.y + 1)


rookMovements =
    [ moveN, moveS, moveE, moveW ]


bishopMovements =
    [ moveNE, moveNW, moveSE, moveSW ]


queenMovements =
    rookMovements ++ bishopMovements


getGameSquareByRow : Int -> Column -> Maybe GameSquare
getGameSquareByRow row columnArray =
    Array.get row columnArray


validatePositionRow : Maybe GameSquare -> Position -> Maybe Position
validatePositionRow gameSquare position =
    case gameSquare of
        Just value ->
            Just position

        Nothing ->
            Nothing


validatePosition : Position -> Board -> Maybe Position
validatePosition position board =
    let
        col =
            Array.get position.y board
    in
    case col of
        Just value ->
            validatePositionRow (getGameSquareByRow position.x value) position

        Nothing ->
            Nothing


getGameSquareMaybe : Maybe Position -> Board -> Maybe GameSquare
getGameSquareMaybe maybePosition board =
    case maybePosition of
        Just position ->
            getGameSquare position board

        Nothing ->
            Nothing


getGameSquare : Position -> Board -> Maybe GameSquare
getGameSquare position board =
    let
        col =
            Array.get position.y board
    in
    case col of
        Just value ->
            getGameSquareByRow position.x value

        Nothing ->
            Nothing


occupiedByPlayer : Player -> Position -> Board -> Bool
occupiedByPlayer p position board =
    let
        maybeSquare =
            getGameSquare position board
    in
    case maybeSquare of
        Just Vacant ->
            False

        Just value ->
            case value of
                Vacant ->
                    False

                Occupied player piece ->
                    p == player

        Nothing ->
            False


emptyPosition : Position -> Board -> Bool
emptyPosition position board =
    let
        maybeSquare =
            getGameSquare position board
    in
    case maybeSquare of
        Just Vacant ->
            True

        _ ->
            False


validateAttackPosition : Player -> Board -> Position -> Maybe Position
validateAttackPosition player board position =
    let
        p =
            validatePosition position board

        theOpponent =
            if player == White then
                Black

            else
                White
    in
    case p of
        Just value ->
            if occupiedByPlayer theOpponent value board then
                p

            else
                Nothing

        Nothing ->
            Nothing


validateEmptyPosition : Board -> Position -> Maybe Position
validateEmptyPosition board position =
    let
        p =
            validatePosition position board
    in
    case p of
        Just value ->
            if emptyPosition value board then
                p

            else
                Nothing

        Nothing ->
            Nothing


maybeAppend : Maybe Position -> List Position -> List Position
maybeAppend maybeThing list =
    case maybeThing of
        Just thing ->
            thing :: list

        Nothing ->
            list


opponent : Player -> Player
opponent player =
    if player == White then
        Black

    else
        White



-- Converts algebraic notation (e.g. "e4") into a Position


fromAlgebraic : String -> Position
fromAlgebraic str =
    let
        value =
            String.uncons str
    in
    case value of
        Just ( c, row ) ->
            Position (97 - Char.toCode c) (8 - Maybe.withDefault -1 (String.toInt row))

        _ ->
            Position -1 -1


toAlgebraic : Position -> String
toAlgebraic position =
    String.cons (Char.fromCode (97 + position.x)) (String.fromInt (8 - position.y))


checkEnPassant : Position -> Position -> Position -> Board -> Maybe Position
checkEnPassant position1 position2 enPassantPosition board =
    let
        canAttack =
            (position1 == enPassantPosition) || (position2 == enPassantPosition)
    in
    if canAttack then
        validateEmptyPosition board enPassantPosition

    else
        Nothing


pawnPositionHelper : Int -> Int -> Player -> Position -> GameModel -> List Position
pawnPositionHelper y originalY player position model =
    let
        attack1 =
            validateAttackPosition player model.board (Position (position.x - 1) (position.y + y))

        attack2 =
            validateAttackPosition player model.board (Position (position.x + 1) (position.y + y))

        one =
            validateEmptyPosition model.board (Position position.x (position.y + y))

        two =
            if (position.y == originalY) && emptyPosition (Position position.x (position.y + y)) model.board then
                validateEmptyPosition model.board (Position position.x (position.y + (y * 2)))

            else
                Nothing

        enPassant =
            checkEnPassant (Position (position.x - 1) (position.y + y))
                (Position (position.x + 1) (position.y + y))
                (fromAlgebraic model.enPassant)
                model.board
    in
    maybeAppend attack1 [] |> maybeAppend attack2 |> maybeAppend one |> maybeAppend two |> maybeAppend enPassant


pawnPositions : Player -> Position -> GameModel -> List Position
pawnPositions player position model =
    let
        y =
            if player == White then
                -1

            else
                1

        originalY =
            if player == White then
                6

            else
                1
    in
    pawnPositionHelper y originalY player position model


rookPositions : Player -> Position -> GameModel -> List Position
rookPositions player position model =
    let
        moves =
            List.map (walk player position model.board []) rookMovements
    in
    List.concat moves


bishopPositions : Player -> Position -> GameModel -> List Position
bishopPositions player position model =
    let
        moves =
            List.map (walk player position model.board []) bishopMovements
    in
    List.concat moves


queenPositions : Player -> Position -> GameModel -> List Position
queenPositions player position model =
    let
        moves =
            List.map (walk player position model.board []) queenMovements
    in
    List.concat moves


identity maybe =
    maybe


removeNothing : List (Maybe Position) -> List Position
removeNothing list =
    List.filterMap identity list


kingMoves position =
    [ Position (position.x - 1) (position.y - 1)
    , Position (position.x - 1) (position.y - 0)
    , Position (position.x - 1) (position.y + 1)
    , Position (position.x - 0) (position.y - 1)
    , Position (position.x - 0) (position.y + 1)
    , Position (position.x + 1) (position.y - 1)
    , Position (position.x + 1) (position.y - 0)
    , Position (position.x + 1) (position.y + 1)
    ]


isVacant : ( Position, GameSquare ) -> Bool
isVacant ( position, gameSquare ) =
    case gameSquare of
        Vacant ->
            True

        _ ->
            False


allAttackFree : Player -> Position -> Position -> Position -> Board -> Bool
allAttackFree player one two three board =
    if not (checkForAttacks player one board) then
        if not (checkForAttacks player two board) then
            not (checkForAttacks player three board)

        else
            False

    else
        False


processQueenCastle : Player -> Position -> GameModel -> List Position
processQueenCastle player position model =
    let
        intermediateSquaresVacant =
            List.all isVacant (findPiecesBy (queenSidePredicate position) model.board)

        one =
            position

        two =
            Position (position.x - 1) position.y

        three =
            Position (position.x - 2) position.y
    in
    if intermediateSquaresVacant && allAttackFree player one two three model.board then
        [ Position (position.x - 2) position.y ]

    else
        []


processKingCastle : Player -> Position -> GameModel -> List Position
processKingCastle player position model =
    let
        intermediateSquaresVacant =
            List.all isVacant (findPiecesBy (kingSidePredicate position) model.board)

        one =
            position

        two =
            Position (position.x + 1) position.y

        three =
            Position (position.x + 2) position.y
    in
    if intermediateSquaresVacant && allAttackFree player one two three model.board then
        [ Position (position.x + 2) position.y ]

    else
        []


kingPositions : Player -> Position -> GameModel -> List Position
kingPositions player position model =
    let
        attackMoves =
            List.map (validateAttackPosition player model.board) (kingMoves position)

        emptyMoves =
            List.map (validateEmptyPosition model.board) (kingMoves position)

        queenSideCastle =
            if player == Black then
                model.blackQueenCastle

            else
                model.whiteQueenCastle

        kingSideCastle =
            if player == Black then
                model.blackKingCastle

            else
                model.whiteKingCastle
    in
    removeNothing attackMoves
        ++ removeNothing emptyMoves
        ++ (if queenSideCastle then
                processQueenCastle player position model

            else
                []
           )
        ++ (if kingSideCastle then
                processKingCastle player position model

            else
                []
           )


knightMoves position =
    [ Position (position.x - 2) (position.y + 1)
    , Position (position.x - 2) (position.y - 1)
    , Position (position.x + 2) (position.y + 1)
    , Position (position.x + 2) (position.y - 1)
    , Position (position.x + 1) (position.y + 2)
    , Position (position.x - 1) (position.y + 2)
    , Position (position.x + 1) (position.y - 2)
    , Position (position.x - 1) (position.y - 2)
    ]


knightPositions : Player -> Position -> GameModel -> List Position
knightPositions player position { board } =
    let
        attackMoves =
            List.map (validateAttackPosition player board) (knightMoves position)

        emptyMoves =
            List.map (validateEmptyPosition board) (knightMoves position)
    in
    removeNothing attackMoves ++ removeNothing emptyMoves


walk : Player -> Position -> Board -> List Position -> MoveFunc -> List Position
walk player position board accum moveFunc =
    let
        nextPosition =
            moveFunc position

        maybeSquare =
            getGameSquare (moveFunc position) board
    in
    case maybeSquare of
        Just value ->
            case value of
                Vacant ->
                    walk player nextPosition board (accum ++ [ nextPosition ]) moveFunc

                Occupied consideringPlayer piece ->
                    if consideringPlayer == opponent player then
                        accum ++ [ nextPosition ]

                    else
                        accum

        Nothing ->
            accum


setGameSquare : Position -> GameSquare -> Board -> Board
setGameSquare position gameSquare board =
    let
        maybeColumn =
            Array.get position.y board
    in
    case maybeColumn of
        Just column ->
            let
                maybeSquare =
                    Array.get position.x column
            in
            case maybeSquare of
                Just destSquare ->
                    Array.set position.y (Array.set position.x gameSquare column) board

                Nothing ->
                    board

        Nothing ->
            board


applyMove : Position -> Position -> Board -> Board
applyMove src dest board =
    let
        maybeSrcSquare =
            getGameSquare src board
    in
    case maybeSrcSquare of
        Just gameSquare ->
            setGameSquare dest gameSquare board |> setGameSquare src Vacant

        Nothing ->
            board


posToString : Position -> String
posToString position =
    String.fromInt position.x ++ " " ++ String.fromInt position.y


logList : List Position -> List Position
logList list =
    let
        log =
            List.map (\n -> Debug.log "move" (posToString n)) list
    in
    list


walkDirection : Player -> Piece -> Position -> Board -> MoveFunc -> Bool
walkDirection player piece position board moveFunc =
    let
        nextPosition =
            moveFunc position

        maybeSquare =
            getGameSquare (moveFunc position) board
    in
    case maybeSquare of
        Just value ->
            case value of
                Vacant ->
                    walkDirection player piece nextPosition board moveFunc

                Occupied consideringPlayer pce ->
                    if (consideringPlayer == opponent player) && pce == piece then
                        True

                    else
                        False

        Nothing ->
            False


lazilyWalkDirection : Player -> Piece -> Position -> Board -> MoveFunc -> Bool -> Bool
lazilyWalkDirection player piece position board moveFunc lastEval =
    if lastEval then
        True

    else
        walkDirection player piece position board moveFunc


facesAttackFrom : Player -> Piece -> List MoveFunc -> Position -> Board -> Bool
facesAttackFrom player piece moveFuncs position board =
    List.foldr (lazilyWalkDirection player piece position board) False moveFuncs


checkForAttacks player position board =
    let
        yDirection =
            if player == White then
                -1

            else
                1
    in
    (List.map (isOpponentPiece player Knight board) (knightMoves position) |> List.any (\v -> v))
        || (isOpponentPiece player Pawn board (Position (position.x - 1) (position.y + yDirection))
                || isOpponentPiece player Pawn board (Position (position.x + 1) (position.y + yDirection))
           )
        || (List.map (isOpponentPiece player King board) (kingMoves position) |> List.any (\v -> v))
        || facesAttackFrom player Rook rookMovements position board
        || facesAttackFrom player Bishop bishopMovements position board
        || facesAttackFrom player Queen queenMovements position board


kingUnderAttack : Player -> Board -> Bool
kingUnderAttack player board =
    let
        maybeKing =
            findPiecesBy (byPlayerPiecePredicate player King) board
                |> List.head
    in
    case maybeKing of
        Just ( position, gameSquare ) ->
            checkForAttacks player position board

        Nothing ->
            False


isOpponentPiece : Player -> Piece -> Board -> Position -> Bool
isOpponentPiece player piece board position =
    let
        other =
            opponent player

        maybeGameSquare =
            getGameSquare position board
    in
    case maybeGameSquare of
        Just gameSquare ->
            case gameSquare of
                Occupied plr pce ->
                    (plr == other) && (pce == piece)

                _ ->
                    False

        Nothing ->
            False



-- Determine if a move of a piece from position src to dest
-- results in a board with check for specified player


moveIntroducesCheck : Position -> Position -> Player -> GameModel -> Bool
moveIntroducesCheck src dest playerInCheck gameModel =
    let
        updatedBoard =
            applyMove src dest gameModel.board
    in
    kingUnderAttack playerInCheck updatedBoard


validMovesFromTuple : GameModel -> ( Position, GameSquare ) -> List Position
validMovesFromTuple model tuple =
    let
        ( position, gameSquare ) =
            tuple
    in
    validMoves position gameSquare model


validMovesPerPiece : GameModel -> ( Position, GameSquare ) -> List ( Position, Position )
validMovesPerPiece model tuple =
    let
        ( position, gameSquare ) =
            tuple
    in
    validMoves position gameSquare model
        |> List.map (\dest -> ( position, dest ))


dumpItem : ( Position, GameSquare ) -> String
dumpItem ( position, gameSquare ) =
    let
        g =
            case gameSquare of
                Vacant ->
                    "V"

                Occupied White Pawn ->
                    "WP"

                Occupied White Rook ->
                    "WR"

                Occupied White King ->
                    "WK"

                Occupied White Queen ->
                    "WQ"

                Occupied White Bishop ->
                    "WB"

                Occupied White Knight ->
                    "WN"

                Occupied Black Pawn ->
                    "BP"

                Occupied Black Rook ->
                    "BR"

                Occupied Black King ->
                    "BK"

                Occupied Black Queen ->
                    "BQ"

                Occupied Black Bishop ->
                    "BB"

                Occupied Black Knight ->
                    "BN"
    in
    "(" ++ posToString position ++ " " ++ g ++ ")  "


dumpList : List ( Position, GameSquare ) -> List ( Position, GameSquare )
dumpList list =
    let
        v =
            List.map dumpItem list |> List.foldr (++) ""
    in
    list


allAvailableMoves : Player -> GameModel -> List ( Position, Position )
allAvailableMoves player model =
    let
        pieces =
            findPiecesBy (piecesByPlayerPredicate player) model.board
    in
    List.map (validMovesPerPiece model) pieces
        |> List.concat
        |> List.filter (\( src, dest ) -> not (moveIntroducesCheck src dest player model))


validMoves : Position -> GameSquare -> GameModel -> List Position
validMoves position gameSquare model =
    case gameSquare of
        Vacant ->
            []

        Occupied player piece ->
            case piece of
                Pawn ->
                    pawnPositions player position model

                Rook ->
                    rookPositions player position model

                Knight ->
                    knightPositions player position model

                Bishop ->
                    bishopPositions player position model

                Queen ->
                    queenPositions player position model

                King ->
                    kingPositions player position model


updateBoard : Board -> GameModel -> GameModel
updateBoard board model =
    { model | board = board }


dropMaybe : Maybe GameSquare -> GameSquare
dropMaybe gameSquare =
    case gameSquare of
        Just value ->
            value

        _ ->
            Vacant


updateGameModel : ( Position, Position ) -> GameModel -> GameModel
updateGameModel ( src, dest ) model =
    let
        destSquare =
            getGameSquare dest model.board |> dropMaybe

        srcSquare =
            getGameSquare src model.board |> dropMaybe

        updatedBoard =
            setGameSquare dest srcSquare model.board
                |> setGameSquare src Vacant
    in
    updateBoard updatedBoard model
        |> toggleWhitesMove
        |> updateHalfMove srcSquare destSquare
        |> updateFullMove srcSquare
        |> updateWhiteCastle src srcSquare
        |> updateBlackCastle src srcSquare
        |> updateEnPassant src dest srcSquare
        |> handlePawnPromotions dest srcSquare
        |> handleEnPassantCapture dest srcSquare model.enPassant
        |> handleCastling src dest srcSquare


updateHalfMove : GameSquare -> GameSquare -> GameModel -> GameModel
updateHalfMove src dest model =
    let
        capture =
            case dest of
                Vacant ->
                    False

                _ ->
                    True

        pawnMove =
            case src of
                Occupied _ Pawn ->
                    True

                _ ->
                    False
    in
    if capture || pawnMove then
        { model | halfMove = 0 }

    else
        { model | halfMove = model.halfMove + 1 }


updateFullMove : GameSquare -> GameModel -> GameModel
updateFullMove src model =
    let
        blackMoved =
            case src of
                Occupied Black _ ->
                    True

                _ ->
                    False
    in
    if blackMoved then
        { model | fullMove = model.fullMove + 1 }

    else
        model


updateWhitesMove : GameSquare -> GameModel -> GameModel
updateWhitesMove src model =
    let
        blackMoved =
            case src of
                Occupied Black _ ->
                    True

                _ ->
                    False
    in
    { model | whitesMove = blackMoved }


updateBlackCastle : Position -> GameSquare -> GameModel -> GameModel
updateBlackCastle position gameSquare model =
    let
        kingMoved =
            case gameSquare of
                Occupied Black King ->
                    True

                _ ->
                    False

        queenRookMoved =
            if (position.x == 0) && (position.y == 0) then
                True

            else
                False

        kingRookMoved =
            if (position.x == 7) && (position.y == 0) then
                True

            else
                False
    in
    if kingMoved then
        { model | blackQueenCastle = False, blackKingCastle = False }

    else if queenRookMoved then
        { model | blackQueenCastle = False }

    else if kingRookMoved then
        { model | blackKingCastle = False }

    else
        model


updateWhiteCastle : Position -> GameSquare -> GameModel -> GameModel
updateWhiteCastle position gameSquare model =
    let
        kingMoved =
            case gameSquare of
                Occupied White King ->
                    True

                _ ->
                    False

        queenRookMoved =
            if (position.x == 0) && (position.y == 7) then
                True

            else
                False

        kingRookMoved =
            if (position.x == 7) && (position.y == 7) then
                True

            else
                False
    in
    if kingMoved then
        { model | whiteQueenCastle = False, whiteKingCastle = False }

    else if queenRookMoved then
        { model | whiteQueenCastle = False }

    else if kingRookMoved then
        { model | whiteKingCastle = False }

    else
        model


updateEnPassant : Position -> Position -> GameSquare -> GameModel -> GameModel
updateEnPassant src dest gameSquare model =
    let
        twoMove =
            abs (dest.y - src.y) == 2
    in
    if twoMove then
        case gameSquare of
            Occupied _ Pawn ->
                { model | enPassant = toAlgebraic dest }

            _ ->
                { model | enPassant = "-" }

    else
        { model | enPassant = "-" }


handlePawnPromotions : Position -> GameSquare -> GameModel -> GameModel
handlePawnPromotions position gameSquare model =
    let
        lastRank =
            (position.y == 0) || (position.y == 7)
    in
    if lastRank then
        case gameSquare of
            Occupied player Pawn ->
                case player of
                    White ->
                        { model | board = setGameSquare position (Occupied White Queen) model.board }

                    Black ->
                        { model | board = setGameSquare position (Occupied Black Queen) model.board }

            _ ->
                model

    else
        model


handleEnPassantCapture : Position -> GameSquare -> String -> GameModel -> GameModel
handleEnPassantCapture position gameSquare enPassant model =
    let
        nowOccupied =
            fromAlgebraic enPassant == position
    in
    if nowOccupied then
        case gameSquare of
            Occupied White Pawn ->
                { model | board = setGameSquare (Position position.x (position.y + 1)) Vacant model.board }

            Occupied Black Pawn ->
                { model | board = setGameSquare (Position position.x (position.y - 1)) Vacant model.board }

            _ ->
                model

    else
        model


setVacant : Player -> Int -> Board -> Board
setVacant player file board =
    let
        y =
            if player == Black then
                0

            else
                7
    in
    setGameSquare (Position file y) Vacant board


setAdjacentSquare : Int -> Position -> GameSquare -> Board -> Board
setAdjacentSquare direction position gameSquare board =
    setGameSquare (Position (position.x + direction) position.y) gameSquare board


handleCastling : Position -> Position -> GameSquare -> GameModel -> GameModel
handleCastling src dest gameSquare model =
    let
        xDiff =
            dest.x - src.x

        absDiff =
            abs (dest.x - src.x)

        opposite =
            -1 * xDiff // 2

        rook =
            if xDiff < 0 then
                0

            else
                7
    in
    if absDiff == 2 then
        case gameSquare of
            Occupied player King ->
                { model | board = setVacant player rook (setAdjacentSquare opposite dest (Occupied player Rook) model.board) }

            _ ->
                model

    else
        model


toggleWhitesMove : GameModel -> GameModel
toggleWhitesMove model =
    { model | whitesMove = not model.whitesMove }
