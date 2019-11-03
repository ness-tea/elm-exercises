{-

Team Member Names and Student Numbers: Vanessa Truong (400023632), Jose Miguel Ballesteros (001411748)

Norman Principle Addressed: Signifers, Feedback, Affordances

Aspect which was not in adherence, or functionality you added to make it a more interesting game which involved this principle:
1.  Feedback: TicTacToe.elm did not provide users with any feedback on when a player wins a game or if the game is a draw. We added logic to the code for
    handling the cases where the Left Player wins, the Right Player wins, and where there's a draw. In any of these cases, an overhead message will come up 
    indicating which case has been reached.

2.  Signifier: While TicTacToe.elm does indicate with an overhead message which player's turn it is during the game (Left Player or Right Player), it is not 
    immediately clear which colour the player corresponds to. To address this, we've added a signifier to indicate to the player what their colour is; 
    when it is the Left Player's turn, the Left Player overhead message will be highlighted in red to indicate that their colour is red in the game, and blue
    will be such the colour for the Right Player.

3.  Affordances: There is currently no way for the players to restart a game (should they wish to restart the game.) A 'Reset' button below the board was added.

Way in which the principle is adhered to by your fix/addition:
1.  Feedback: Users can get feedback on who won the game, or if the game is a draw.

2.  Signifier: The game has a signifier that communicates to the user which colour they are on the tic tac toe board.

3.  Affordances: A button to reset the board allows users to replay the game at any point during the current session. 

-}

module App exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)

myShapes model = [ square 10
                     |> filled (boxColour model.box11)
                     |> notifyTap Click11
                     |> move (-12,12)
                 , square 10
                     |> filled (boxColour model.box12)
                     |> notifyTap Click12
                     |> move (0,12)
                 , square 10
                     |> filled (boxColour model.box13)
                     |> notifyTap Click13
                     |> move (12,12)
                 , square 10
                     |> filled (boxColour model.box21)
                     |> notifyTap Click21
                     |> move (-12,0)
                 , square 10
                     |> filled (boxColour model.box22)
                     |> notifyTap Click22
                     |> move (0,0)
                 , square 10
                     |> filled (boxColour model.box23)
                     |> notifyTap Click23
                     |> move (12,0)
                 , square 10
                     |> filled (boxColour model.box31)
                     |> notifyTap Click31
                     |> move (-12,-12)
                 , square 10
                     |> filled (boxColour model.box32)
                     |> notifyTap Click32
                     |> move (0,-12)
                 , square 10
                     |> filled (boxColour model.box33)
                     |> notifyTap Click33
                     |> move (12,-12)
                 , playerBanner model |> move (0,50)
                 , text (String.fromInt <| round model.time) |> filled black
                     |> move (80,50)
                 , text "Reset" 
                     |> size 8
                     |> filled black
                     |> notifyTap Clear
                     |> move (-8,-30)
                 --, gameOver model
                 ]

-- Jose's Stuff
type Status = LeftWin | RightWin | LeftNext | RightNext | Draw

-- Jose's Stuff
gameOver model =
  case (victory model) of
      LeftWin ->  text "Left Wins!" |> centered |> filled (rgb 0 0 255)
      RightWin -> text "Right Wins!" |> centered |> filled (rgb 255 0 0)
      LeftNext -> text "Left Player" |> centered |> filled (rgb 0 0 255)
      RightNext -> text "Right Player"|> centered |> filled (rgb 255 0 0)
      Draw -> text "Draw Game" |> centered |> filled black

-- display a victory banner
-- use logical and &&
-- and logical or  ||
-- (and ()s)
-- Jose's Stuff built on the OG shit
victory model =
  if model.box11 == model.box12 && model.box12 == model.box13 && model.box11 /= EmptyBox then
      if model.box11 == LeftBox then LeftWin else RightWin
  else if model.box21 == model.box22 && model.box22 == model.box23 && model.box21 /= EmptyBox then
      if model.box21 == LeftBox then LeftWin else RightWin
  else if model.box31 == model.box32 && model.box32 == model.box33 && model.box31 /= EmptyBox then
      if model.box31 == LeftBox then LeftWin else RightWin
  else if model.box11 == model.box21 && model.box21 == model.box31 && model.box11 /= EmptyBox then
      if model.box11 == LeftBox then LeftWin else RightWin
  else if model.box12 == model.box22 && model.box22 == model.box32 && model.box12 /= EmptyBox then
      if model.box12 == LeftBox then LeftWin else RightWin
  else if model.box13 == model.box23 && model.box23 == model.box33 && model.box13 /= EmptyBox then
      if model.box13 == LeftBox then LeftWin else RightWin
  else if model.box11 == model.box22 && model.box22 == model.box33 && model.box11 /= EmptyBox then
      if model.box11 == LeftBox then LeftWin else RightWin
  else if model.box13 == model.box22 && model.box22 == model.box31 && model.box13 /= EmptyBox then
      if model.box13 == LeftBox then LeftWin else RightWin
  else if model.box11 /= EmptyBox && model.box12 /= EmptyBox && model.box13 /= EmptyBox && 
          model.box21 /= EmptyBox && model.box22 /= EmptyBox && model.box23 /= EmptyBox && 
          model.box31 /= EmptyBox && model.box32 /= EmptyBox && model.box33 /= EmptyBox then
      Draw
  else
      if model.player == Left then LeftNext else RightNext

-- draw a box with different colours for different players
boxColour box =
  case box of
    LeftBox -> rgb 0 0 255
    RightBox -> rgb 255 0 0
    EmptyBox -> rgb 212 212 212

-- telling the current player they can click
playerBanner model =
  case model.player of
    Left  -> gameOver model
    Right -> gameOver model

-- to know when a click happens, we need to define new messages
type Msg = Tick Float GetKeyState
         | Click11
         | Click12
         | Click13
         | Click21
         | Click22
         | Click23
         | Click31
         | Click32
         | Click33
         | Clear
         
type Player = Left | Right

swapPlayer player =
  case player of
    Left  -> Right
    Right -> Left

type Box = LeftBox | RightBox | EmptyBox

update msg model = case msg of
                     Tick t _ -> { model | time = t }
                     Click11  -> { model | box11 = changeBox model.player model.box11
                                         , player = swapPlayer model.player
                                         }
                     Click12  -> { model | box12 = changeBox model.player model.box12
                                         , player = swapPlayer model.player
                                         }
                     Click13  -> { model | box13 = changeBox model.player model.box13
                                         , player = swapPlayer model.player
                                         }
                     Click21  -> { model | box21 = changeBox model.player model.box21
                                         , player = swapPlayer model.player
                                         }
                     Click22  -> { model | box22 = changeBox model.player model.box22
                                         , player = swapPlayer model.player
                                         }
                     Click23  -> { model | box23 = changeBox model.player model.box23
                                         , player = swapPlayer model.player
                                         }
                     Click31  -> { model | box31 = changeBox model.player model.box31
                                         , player = swapPlayer model.player
                                         }
                     Click32  -> { model | box32 = changeBox model.player model.box32
                                         , player = swapPlayer model.player
                                         }
                     Click33  -> { model | box33 = changeBox model.player model.box33
                                         , player = swapPlayer model.player
                                         }
                     Clear    -> { model | time = 0
                                         , player = Left
                                         , box11 = EmptyBox
                                         , box12 = EmptyBox
                                         , box13 = EmptyBox
                                         , box21 = EmptyBox
                                         , box22 = EmptyBox
                                         , box23 = EmptyBox
                                         , box31 = EmptyBox
                                         , box32 = EmptyBox
                                         , box33 = EmptyBox
                                         }

init = { time = 0
       , player = Left
       , box11 = EmptyBox
       , box12 = EmptyBox
       , box13 = EmptyBox
       , box21 = EmptyBox
       , box22 = EmptyBox
       , box23 = EmptyBox
       , box31 = EmptyBox
       , box32 = EmptyBox
       , box33 = EmptyBox
       }

changeBox player box = 
  case (player, box) of
    (Left, EmptyBox) -> LeftBox
    (Right,EmptyBox) -> RightBox
    otherwise        -> box
             

main = gameApp Tick {model = init, view = view, update = update, title = "Game Slot"}
view model = collage 192 128 (myShapes model)
            
