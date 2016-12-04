module Core exposing (..)

import Array exposing (Array)
import Char exposing (..)


type Player
    = White
    | Black


type Piece
    = Pawn
    | Rook
    | Bishop
    | Knight
    | Queen
    | King


type GameSquare
    = Vacant
    | Occupied Player Piece


type alias Column =
    Array GameSquare


type alias Board =
    Array Column


type alias Position =
    { x : Int, y : Int }


type alias Move =
    ( Position, Position )


type alias Moves =
    List Move


type alias GameModel =
    { board : Board
    , whitesMove : Bool
    , whiteQueenCastle : Bool
    , whiteKingCastle : Bool
    , blackQueenCastle : Bool
    , blackKingCastle : Bool
    , enPassant : String
    , halfMove : Int
    , fullMove : Int
    }
