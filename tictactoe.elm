module TicTacToe where

import Graphics.Input as Input

{-- Types --}
data Player         = X | O
type Marker         = Maybe Player

type Square         = { row: Int, col: Int, content: Marker }

type TicTacToeBoard = { r1c1: Square, r1c2: Square, r1c3: Square
                      , r2c1: Square, r2c2: Square, r2c3: Square
                      , r3c1: Square, r3c2: Square, r3c3: Square }

data State          = Starting | Playing | Done

type GameState      = { board: TicTacToeBoard, active: Player
                      , winner: Maybe Player, state: State }

type Inputs         = { input: Signal Int }

{-- Util Funcs --}

{-- Data --}
buttonGroup = Input.buttons 0

gameInput : Inputs
gameInput = { input = buttonGroup.events }

makeSquare : Int -> Int -> Square
makeSquare r c = { row = r, col = c, content = Nothing }

getRows : TicTacToeBoard -> [ [ Square ] ]
getRows board = [ [ board.r1c1, board.r1c2, board.r1c3 ]
                , [ board.r2c1, board.r2c2, board.r2c3 ]
                , [ board.r3c1, board.r3c2, board.r3c3 ] ]

getCols : TicTacToeBoard -> [ [ Square ] ]
getCols board = [ [ board.r1c1, board.r2c1, board.r3c1 ]
                , [ board.r1c2, board.r2c2, board.r3c2 ]
                , [ board.r1c3, board.r2c3, board.r3c3 ] ]

getDiags : TicTacToeBoard -> [ [ Square ] ]
getDiags board = [ [ board.r1c1, board.r2c2, board.r3c3 ]
                 , [ board.r1c3, board.r2c2, board.r3c1 ] ]

getSquareAccessor : Int -> Int -> (TicTacToeBoard -> Square)
getSquareAccessor row col =
  case (row, col) of
    (1, 1) -> .r1c1
    (1, 2) -> .r1c2
    (1, 3) -> .r1c3
    (2, 1) -> .r2c1
    (2, 2) -> .r2c2
    (2, 3) -> .r2c3
    (3, 1) -> .r3c1
    (3, 2) -> .r3c2
    (3, 3) -> .r3c3

coordsToVal : ( Int, Int ) -> Int
coordsToVal ( row, col ) = ( row * 10 ) + col

valToCoords : Int -> ( Int, Int )
valToCoords val = ( val `div` 10, val `rem` 10 )


initBoard : TicTacToeBoard
initBoard = { r1c1 = makeSquare 1 1, r1c3 = makeSquare 1 3, r1c2 = makeSquare 1 2
            , r2c1 = makeSquare 2 1, r2c2 = makeSquare 2 2, r2c3 = makeSquare 2 3
            , r3c1 = makeSquare 3 1, r3c2 = makeSquare 3 2, r3c3 = makeSquare 3 3 }

initGame : GameState
initGame = { board = initBoard, active = X, winner = Nothing, state = Starting }

{-- Logic --}
progressGame : Int -> GameState -> GameState
progressGame input gs =
  case input of
    0   ->  { gs | state <- Starting }
    100 ->  { gs | state <- Playing  }
    200 ->  { gs | state <- Done     }
    300 ->  initGame

    n   ->  let newBoard = updateBoard gs.board n gs.active
                winner   = checkBoard  newBoard
            in case winner of
              Nothing -> { gs | board   <-  newBoard
                              , active  <-  case gs.active of
                                              O -> X
                                              X -> O }
              Just p  -> { gs | board   <-  newBoard
                              , winner  <-  winner
                              , state   <-  Done }

updateBoard : TicTacToeBoard -> Int -> Player -> TicTacToeBoard
updateBoard board val player =
  let (row,col) = valToCoords val
      accessor  = getSquareAccessor row col
      square    = board |> accessor
      newSq     = { square | content <- Just player }
  in case (row,col) of
    (1, 1) -> { board | r1c1 <- newSq }
    (1, 2) -> { board | r1c2 <- newSq }
    (1, 3) -> { board | r1c3 <- newSq }
    (2, 1) -> { board | r2c1 <- newSq }
    (2, 2) -> { board | r2c2 <- newSq }
    (2, 3) -> { board | r2c3 <- newSq }
    (3, 1) -> { board | r3c1 <- newSq }
    (3, 2) -> { board | r3c2 <- newSq }
    (3, 3) -> { board | r3c3 <- newSq }

checkBoard : TicTacToeBoard -> Maybe Player
checkBoard board = Nothing

checkRows : TicTacToeBoard -> Marker
checkRows board =
  let rows = getRows board
  in Nothing

samePlayer : [ Square ] -> Marker
samePlayer squares = Nothing

{-- Graphics --}
drawState : GameState -> Element
drawState { board, active, winner, state } =
  let elem =
    if | state == Starting -> drawStart
       | state == Playing  -> drawGame board active
       | state == Done     -> asText "Done"
  in container 600 300 middle elem

drawGame : TicTacToeBoard -> Player -> Element
drawGame board player =
  flow down [ "Turn: Player " ++ (show player) |> toText |> Text.height 24.0 |> text
            , drawBoard board ]

drawBoard : TicTacToeBoard -> Element
drawBoard board =
  flow down <| map (flow right) <| map ( map drawSquare ) <| getRows board

drawSquare : Square -> Element
drawSquare { row, col, content } =
  case content of
    Nothing -> container 40 40 middle ( buttonGroup.button ((row * 10) + col) " ? " )
    Just p  -> container 40 40 middle ( show p |> toText |> Text.height 30.0 |> text )

drawStart : Element
drawStart =
  flow right [ "Welcome to Tic-Tac-Toe" |> toText |> Text.color green |> Text.height 36.0 |> text
             , spacer 50 1
             , buttonGroup.button 100 "Start" ]

{-- Main --}
main = drawState <~ foldp progressGame initGame (gameInput.input)