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
                 , playerBanner model.player |> move (0,50)
                 , text (String.fromInt <| round model.time) |> filled black
                     |> move (80,50)
                 , text (victory model) |> size 30 |> centered |> filled yellow
                 ]

-- display a victory banner
-- use logical and &&
-- and logical or  ||
-- (and ()s)
victory model =
  if model.box11 == model.box12 && model.box12 == model.box13 && model.box11 /= EmptyBox
    then
      "Somebody won!"
    else
      ""

-- draw a box with different colours for different players
boxColour box =
  case box of
    LeftBox -> rgb 0 0 255
    RightBox -> rgb 255 0 0
    EmptyBox -> rgb 255 0 255

-- telling the current player they can click
playerBanner player =
  case player of
    Left  -> text "Left Player" |> centered |> filled black 
    Right -> text "Right Player" |> centered |> filled black

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
    