module Game exposing (Model, Msg(..), Point, appendMoveHistory, boardWithPieces, chooseImage, clearMoves, clearSelected, col, color, combine, determineMoves, executeEngine, filterCheckConstrained, init, isBlack, isEven, isOdd, main, moveHistory, onMouseUp, padList, pieceByCol, processMouseUp, processMove, px, renderBlankBoard, renderHighlight, renderPiece, renderPieces, renderPlacement, renderPotential, renderSelected, setSelected, squareSize, startEngine, subscriptions, tile, toChessPosition, toColor, toPoint, update, updateCheck, updateCheckMate, updateMovesForSelected, updateSquareClicked, view)

import Array exposing (..)
import Browser exposing (..)
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Nav
import Core as Chess exposing (..)
import Engine as Engine exposing (..)
import FEN as FEN exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Json exposing (..)
import Move as Move exposing (..)
import SAN as SAN exposing (..)
import Task exposing (..)
import Time exposing (..)
import Url exposing (Url)


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
    = SquareClicked ( Float, Float )
    | StartEngine
    | ComputerMove (Maybe Move)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


squareSize =
    64


type alias Flags =
    { gameState : String
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( Model (FEN.toModel initialBoard) [] Nothing False False [] False, Cmd.none )


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.computerThinking && not model.game.whitesMove then
        Time.every 50 startEngine

    else
        Sub.none


startEngine : Posix -> Msg
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
            updateSquareClicked (fromFloatPair position) model

        StartEngine ->
            ( { model | computerThinking = True }, executeEngine model )

        ComputerMove maybeMove ->
            case maybeMove of
                Just ( srcPosition, destPosition ) ->
                    ( processMove srcPosition destPosition model, Cmd.none )

                Nothing ->
                    ( { model | checkMate = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )


clearSelected : Model -> Model
clearSelected model =
    { model | selected = Nothing }


setSelected : Chess.Position -> Model -> Model
setSelected position model =
    { model | selected = Just position }


clearMoves : Model -> Model
clearMoves model =
    { model | potentialMoves = [] }


toChessPosition : Position -> Chess.Position
toChessPosition position =
    Chess.Position (position.x // squareSize) (position.y // squareSize)


fromFloatPair : ( Float, Float ) -> Chess.Position
fromFloatPair ( x, y ) =
    Chess.Position (floor (x / squareSize)) (floor (y / squareSize))


filterCheckConstrained : Player -> Chess.Position -> GameModel -> List Chess.Position -> List Chess.Position
filterCheckConstrained player src model availableMoves =
    List.filter (\dest -> not (Move.moveIntroducesCheck src dest player model)) availableMoves


determineMoves : Player -> Maybe Chess.Position -> GameSquare -> GameModel -> List Chess.Position
determineMoves player maybePosition gameSquare model =
    case maybePosition of
        Just position ->
            Move.validMoves position gameSquare model |> filterCheckConstrained player position model

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
    if model.game.whitesMove && (whiteMoves == 0) then
        { model | checkMate = True }

    else if blackMoves == 0 then
        { model | checkMate = True }

    else
        model


updateCheck : Model -> Model
updateCheck model =
    let
        player =
            if model.game.whitesMove then
                White

            else
                Black
    in
    { model | checkMate = Move.kingUnderAttack player model.game.board }


appendMoveHistory : HalfMove -> Model -> Model
appendMoveHistory halfMove model =
    { model | moves = model.moves ++ [ halfMove ] }


processMove : Chess.Position -> Chess.Position -> Model -> Model
processMove src dest model =
    let
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
        |> appendMoveHistory moveAsSAN


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

            else if List.length potential == 1 then
                processMove selectedValue chessPosition model

            else
                case gameSquare of
                    Just squareValue ->
                        case squareValue of
                            Vacant ->
                                model

                            Occupied player piece ->
                                if player == White then
                                    model |> setSelected chessPosition |> updateMovesForSelected

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
                                model |> setSelected chessPosition |> updateMovesForSelected

                            else if (player == Black) && not model.game.whitesMove then
                                model |> setSelected chessPosition |> updateMovesForSelected

                            else
                                model

                Nothing ->
                    model


px : Int -> String
px value =
    String.fromInt value ++ "px"


isBlack : Int -> Int -> Bool
isBlack x y =
    let
        total =
            x + y

        rem =
            remainderBy 2 total
    in
    rem == 0


color x y =
    let
        black =
            isBlack x y
    in
    if isBlack x y then
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
        , style "backgroundColor" backgroundColor
        , style "width" (px squareSize)
        , style "height" (px squareSize)
        , style "position" "absolute"
        , style "left" xStr
        , style "top" yStr
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
    in
    div
        [ onMouseUp
        , style "width" (px (squareSize - 6))
        , style "height" (px (squareSize - 6))
        , style "position" "absolute"
        , style "left" point.x
        , style "top" point.y
        , style "borderStyle" borderStyle
        , style "borderWidth" "6"
        , style "borderColor" colorStr
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
            "images/" ++ toColor player ++ "p.png"

        Knight ->
            "images/" ++ toColor player ++ "n.png"

        Bishop ->
            "images/" ++ toColor player ++ "b.png"

        Rook ->
            "images/" ++ toColor player ++ "r.png"

        Queen ->
            "images/" ++ toColor player ++ "q.png"

        King ->
            "images/" ++ toColor player ++ "k.png"


renderPiece player piece x y =
    let
        xPos =
            String.fromInt (x * squareSize) ++ "px"

        yPos =
            String.fromInt (y * squareSize) ++ "px"
    in
    Html.img
        [ onMouseUp
        , Html.Attributes.src (chooseImage player piece)
        , style "cursor" "grab"
        , style "width" (px squareSize)
        , style "height" (px squareSize)
        , style "position" "absolute"
        , style "left" xPos
        , style "top" yPos
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
                ++ renderSelected model.selected
                ++ renderPotential model.potentialMoves
                ++ renderPieces model.game.board
    in
    div []
        all


onMouseUp : Attribute Msg
onMouseUp =
    Mouse.onUp (\e -> SquareClicked e.clientPos)


padList : List HalfMove -> List HalfMove
padList list =
    let
        odd =
            remainderBy 2 (List.length list) == 1
    in
    if odd then
        list ++ [ "" ]

    else
        list


isOdd : ( Int, HalfMove ) -> Bool
isOdd ( i, h ) =
    remainderBy 2 i == 1


isEven : ( Int, HalfMove ) -> Bool
isEven ( i, h ) =
    remainderBy 2 i == 0


combine : ( Int, HalfMove ) -> ( Int, HalfMove ) -> Html Msg
combine ( _, a ) ( _, b ) =
    li [] [ text (b ++ "  " ++ a) ]


moveHistory : Model -> Html Msg
moveHistory model =
    let
        toTuple i m =
            ( i, m )

        paddedList =
            padList model.moves

        whiteMoves =
            List.filter isOdd (List.indexedMap toTuple paddedList)

        blackMoves =
            List.filter isEven (List.indexedMap toTuple paddedList)

        items =
            List.map2 combine whiteMoves blackMoves
    in
    div
        [ style "position" "absolute"
        , style "left" "520px"
        , style "top" "20px"
        ]
        [ ol
            []
            items
        ]


view : Model -> Document Msg
view model =
    let
        body =
            [ div
                []
                [ boardWithPieces model
                , moveHistory model
                ]
            ]
    in
    { title = "Elm Chess"
    , body = body
    }
