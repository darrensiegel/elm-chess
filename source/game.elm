module Chess.Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Chess.Core as Chess exposing (..)
import Chess.Move as Move exposing (..)
import Chess.FEN as FEN exposing (..)
import Chess.SAN as SAN exposing (..)
import Chess.Engine as Engine exposing (..)
import Array exposing (..)
import Json.Decode as Json exposing (..)
import Mouse exposing (..)
import Debug exposing (..)
import Task exposing (..)
import Time exposing (..)


type alias Point =
    { x : String
    , y : String
    }


type alias Model =
    { game : Chess.GameModel
    , potentialMoves : List Chess.Position
    , selected : Maybe Chess.Position
    , check : Bool
    , checkMate : Bool
    , moves : AllMoves
    , computerThinking : Bool
    }


type Msg
    = SquareClicked Mouse.Position
    | StartEngine
    | ComputerMove (Maybe Move)


squareSize =
    64


init : ( Model, Cmd Msg )
init =
    Model (FEN.toModel initialBoard) [] Nothing False False [] False ! []


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if ((not model.computerThinking && not model.game.whitesMove)) then
        Time.every (millisecond * 50) startEngine
    else
        Sub.none


startEngine : Time -> Msg
startEngine time =
    StartEngine


executeEngine : Model -> Cmd Msg
executeEngine model =
    Task.perform ComputerMove (Engine.nextMove model.game)


updateSquareClicked : Chess.Position -> Model -> ( Model, Cmd Msg )
updateSquareClicked position model =
    let
        nextModel =
            processMouseUp position model
    in
        case nextModel.game.whitesMove of
            False ->
                ( nextModel, Cmd.none )

            True ->
                ( nextModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquareClicked position ->
            updateSquareClicked (toChessPosition position) model

        StartEngine ->
            { model | computerThinking = True } ! [ executeEngine model ]

        ComputerMove maybeMove ->
            case maybeMove of
                Just ( srcPosition, destPosition ) ->
                    processMove srcPosition destPosition model ! []

                Nothing ->
                    { model | checkMate = True } ! []


clearSelected : Model -> Model
clearSelected model =
    { model | selected = Nothing }


setSelected : Chess.Position -> Model -> Model
setSelected position model =
    { model | selected = Just position }


clearMoves : Model -> Model
clearMoves model =
    { model | potentialMoves = [] }


toChessPosition : Mouse.Position -> Chess.Position
toChessPosition position =
    Chess.Position (position.x // squareSize) (position.y // squareSize)


filterCheckConstrained : Player -> Chess.Position -> GameModel -> List Chess.Position -> List Chess.Position
filterCheckConstrained player src model availableMoves =
    List.filter (\dest -> not (Move.moveIntroducesCheck src dest player model)) availableMoves


determineMoves : Player -> Maybe Chess.Position -> GameSquare -> GameModel -> List Chess.Position
determineMoves player maybePosition gameSquare model =
    case maybePosition of
        Just position ->
            (Move.validMoves position gameSquare model) |> (filterCheckConstrained player position model)

        Nothing ->
            []


updateMovesForSelected : Model -> Model
updateMovesForSelected model =
    let
        maybeGameSquare =
            Move.getGameSquareMaybe model.selected model.game.board
    in
        case maybeGameSquare of
            Just gameSquare ->
                { model | potentialMoves = determineMoves White model.selected gameSquare model.game }

            Nothing ->
                clearMoves model


updateCheckMate : Model -> Model
updateCheckMate model =
    let
        whiteMoves =
            List.length (Move.allAvailableMoves White model.game)

        blackMoves =
            List.length (Move.allAvailableMoves Black model.game)
    in
        if (model.game.whitesMove && (whiteMoves == 0)) then
            { model | checkMate = True }
        else if (blackMoves == 0) then
            { model | checkMate = True }
        else
            model


updateCheck : Model -> Model
updateCheck model =
    let
        player =
            if (model.game.whitesMove) then
                White
            else
                Black
    in
        { model | checkMate = (Move.kingUnderAttack player model.game.board) }


appendMoveHistory : HalfMove -> Model -> Model
appendMoveHistory halfMove model =
    { model | moves = (model.moves ++ [ halfMove ]) }


processMove : Chess.Position -> Chess.Position -> Model -> Model
processMove src dest model =
    let
        log =
            Debug.log "processMove" (posToString dest)

        updatedGameModel =
            updateGameModel ( src, dest ) model.game

        moveAsSAN =
            SAN.toHalfMove ( src, dest ) model.game
    in
        { model | game = updatedGameModel, computerThinking = False }
            |> clearSelected
            |> clearMoves
            |> updateCheckMate
            |> updateCheck
            |> (appendMoveHistory moveAsSAN)


processMouseUp : Chess.Position -> Model -> Model
processMouseUp chessPosition model =
    let
        gameSquare =
            Move.getGameSquare chessPosition model.game.board

        potential =
            List.filter (\item -> item == chessPosition) model.potentialMoves
    in
        case model.selected of
            Just selectedValue ->
                if chessPosition == selectedValue then
                    clearSelected model |> clearMoves
                else if (List.length potential == 1) then
                    processMove selectedValue chessPosition model
                else
                    case gameSquare of
                        Just squareValue ->
                            case squareValue of
                                Vacant ->
                                    model

                                Occupied player piece ->
                                    if player == White then
                                        model |> (setSelected chessPosition) |> updateMovesForSelected
                                    else
                                        model

                        Nothing ->
                            model

            Nothing ->
                case gameSquare of
                    Just squareValue ->
                        case squareValue of
                            Vacant ->
                                model

                            Occupied player piece ->
                                if (player == White) && model.game.whitesMove then
                                    model |> (setSelected chessPosition) |> updateMovesForSelected
                                else if (player == Black) && (not model.game.whitesMove) then
                                    model |> (setSelected chessPosition) |> updateMovesForSelected
                                else
                                    model

                    Nothing ->
                        model


px : Int -> String
px value =
    (toString value) ++ "px"


isBlack : Int -> Int -> Bool
isBlack x y =
    (rem (x + y) 2) == 1


color x y =
    let
        black =
            isBlack x y
    in
        if (isBlack x y) then
            "#D8A367"
        else
            "#E8E5E1"


tile : Int -> Int -> Html Msg
tile xPos yPos =
    let
        xStr =
            (xPos * squareSize) |> px

        yStr =
            (yPos * squareSize) |> px

        backgroundColor =
            color xPos yPos
    in
        div
            [ onMouseUp
            , style
                [ "backgroundColor" => backgroundColor
                , "width" => px squareSize
                , "height" => px squareSize
                , "position" => "absolute"
                , "left" => xStr
                , "top" => yStr
                ]
            ]
            []


col x =
    List.map (tile x) (List.range 0 7)


toPoint : Chess.Position -> Point
toPoint position =
    Point (px (position.x * squareSize))
        (px (position.y * squareSize))


renderHighlight : String -> String -> Chess.Position -> Html Msg
renderHighlight colorStr borderStyle position =
    let
        point =
            toPoint position

        borderWidth =
            6
    in
        div
            [ onMouseUp
            , style
                [ "width" => px (squareSize - borderWidth)
                , "height" => px (squareSize - borderWidth)
                , "position" => "absolute"
                , "left" => point.x
                , "top" => point.y
                , "borderStyle" => borderStyle
                , "borderWidth" => toString borderWidth
                , "borderColor" => colorStr
                ]
            ]
            []


renderBlankBoard =
    List.map col (List.range 0 7)
        |> List.concat


toColor player =
    case player of
        White ->
            "w"

        Black ->
            "b"


chooseImage player piece =
    case piece of
        Pawn ->
            "images/" ++ toColor (player) ++ "p.png"

        Knight ->
            "images/" ++ toColor (player) ++ "n.png"

        Bishop ->
            "images/" ++ toColor (player) ++ "b.png"

        Rook ->
            "images/" ++ toColor (player) ++ "r.png"

        Queen ->
            "images/" ++ toColor (player) ++ "q.png"

        King ->
            "images/" ++ toColor (player) ++ "k.png"


(=>) =
    (,)


renderPiece player piece x y =
    let
        xPos =
            (toString (x * squareSize)) ++ "px"

        yPos =
            (toString (y * squareSize)) ++ "px"
    in
        Html.img
            [ onMouseUp
            , Html.Attributes.src (chooseImage player piece)
            , style
                [ "cursor" => "grab"
                , "width" => px squareSize
                , "height" => px squareSize
                , "position" => "absolute"
                , "left" => xPos
                , "top" => yPos
                ]
            ]
            []


renderPlacement : Int -> Int -> GameSquare -> Html Msg
renderPlacement y x p =
    case p of
        Vacant ->
            div [] []

        Occupied player piece ->
            renderPiece player piece x y


pieceByCol y row =
    Array.toList row
        |> List.indexedMap (renderPlacement y)


renderPieces : Board -> List (Html Msg)
renderPieces board =
    Array.toList board
        |> List.indexedMap pieceByCol
        |> List.concat


renderSelected : Maybe Chess.Position -> List (Html Msg)
renderSelected maybePosition =
    case maybePosition of
        Just value ->
            [ renderHighlight "#5495B6" "solid" value ]

        Nothing ->
            []


renderPotential : List Chess.Position -> List (Html Msg)
renderPotential positions =
    List.map (renderHighlight "#C72626" "dashed") positions


boardWithPieces model =
    let
        all =
            renderBlankBoard
                ++ (renderSelected model.selected)
                ++ (renderPotential model.potentialMoves)
                ++ (renderPieces model.game.board)
    in
        div []
            all


onMouseUp : Attribute Msg
onMouseUp =
    on "mouseup" (Json.map SquareClicked Mouse.position)


padList : List HalfMove -> List HalfMove
padList list =
    let
        isOdd =
            (rem (List.length list) 2 == 1)
    in
        if (isOdd) then
            list ++ [ "" ]
        else
            list


isOdd : ( Int, HalfMove ) -> Bool
isOdd ( i, h ) =
    (rem i 2 == 1)


isEven : ( Int, HalfMove ) -> Bool
isEven ( i, h ) =
    (rem i 2 == 0)


combine : ( Int, HalfMove ) -> ( Int, HalfMove ) -> Html Msg
combine ( _, a ) ( _, b ) =
    li [] [ text (b ++ "  " ++ a) ]


moveHistory : Model -> Html Msg
moveHistory model =
    let
        paddedList =
            padList model.moves

        whiteMoves =
            (List.filter isOdd (List.indexedMap (,) paddedList))

        blackMoves =
            (List.filter isEven (List.indexedMap (,) paddedList))

        items =
            List.map2 combine whiteMoves blackMoves
    in
        div
            [ style
                [ "position" => "absolute"
                , "left" => "520px"
                , "top" => "20px"
                ]
            ]
            [ ol
                []
                items
            ]


view : Model -> Html Msg
view model =
    div
        []
        [ (boardWithPieces model)
        , (moveHistory model)
        ]
