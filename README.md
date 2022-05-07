# elm-chess

A human versus computer chess game, entirely in Elm. Nothing fancy: play it live [here](https://s3.amazonaws.com/elm-chess.com/index.html).

## Background

I was inspired to write a chess engine after watching a lecture on the minimax algorithm and alpha beta pruning from 
[MIT's AI course](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-034-artificial-intelligence-fall-2010/).  I have been itching to try out Elm in a non-trivial project, so I eschewed my usual React/Redux stack and dove into
Elm. 

## Details

My original plan was to implement the chess engine server side, which then a client application (written in Elm) would
send requests to in order to receive the computer generated moves. After digging in and building the beginnings of the client, I was enjoying the development
enough that I continued and implemented everything in Elm.

### Module `Engine`

Module `Engine` contains an implementation of minimax with alpha-beta pruning.  The engine, as it is checked in right
now, only explores two levels (the computer move and a subsequent human move).  On my MacBook Pro it takes only a second
or two to generate a move.  Increasing the depth of search to three levels is still fairly playable, though moves take 
closer to twenty or thirty seconds to calculate.  At this point the browser will sometimes show a long running script warning.

### Module `Eval`

Board evaulation was perhaps the most fun aspect of the game to implement. Piece count, control of center, pawn structure,
as well as threatened pieces all factor in to the strength of board evaluation. There are still some ideas that I'd like to
explore such as piece weightings that change according to the game phase.  As it stands right now, the evaluation
function yields a computer player that is quite aggressive.   

### Module `FEN`

Module for parsing Forsyth-Edwards Notation. My original intent was to use FEN as the format for requests to the server. After
going completely client-side, I found FEN still useful for specifying initial starting boards for debugging and testing.

## Running from source

Build the JS:

```
$ elm-make source/Game.elm --output=elm.js
```

And run it

```
$ elm-reactor
```

## Feedback
If you have any feedback or suggestions I'd love to hear them. 
