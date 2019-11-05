{-

Submitted By: Jose Miguel Ballesteros (001411748) & Vanessa Truong (400023632)

User:   Our implementation of SinCreator is intended to be used by grade 10 student's just being introduced to trigonometry
        for the very first time. Together we recalled our past experiences from when we were in that situation to design a graph
        that helps strengthen our understanding on the fundamentals of Sine and Cosine waves.

Activity:   Our typical user will be engaged in learning the fundamentals of Sine & Cosine waves. Our app is designed to 
            help students who are having difficulty comprehending what is being taught to them in class by presenting them with a
            visual aid through the means of this revised app.

Emotion:    The app's use of bold neon colours should evoke bright feelings in the user. Its spacious template and simple aesthetic should 
            work to destress the user, void the user of any chaotic thoughts, and help to break down a more complex mathematical 
            model into an understandable, basic model. 

Tasks:  Some tasks our user's will partake in are:
            -   Doing homework that cover graphing Sine & Cosine functions, including the transformations with different colors
                for amplitude and phase shift changes
            -   Studying for tests & exams, our graphing tool is a perfect study tool that can help student's confirm their 
                knowledge or provide a starting point for those who don't know where to start.

Typical Interaction: The user will open the app, and read the instructions described in the black outlined box at the bottom of the app page. They will click the 
                     coloured arrows located in the centre of the screen, and observe the changes to the sine and cosine waves on the horizontal and 
                     vertical axes accordingly. They can take note of the scale at the top left corner of the web page, and see the scalable increase/decrease in the 
                     amplitude, wavelength, and phase shift. They can also take note of the original sine and cosine wave amplitude rendered in grey on both of the axes.

Principle 1:    Conceptual Model is the first Norman principle that has guided our redesign of SinCreator. There are coloured arrows for altering the amplitude, period and phase shift of
                the wave equation and the coloured lines located on the circle in the top left corner of the app page. The coloured lines on the circle, which helps us with drawing
                the waves onto the axes, represent the magnitude for the amplitude, wavelength, and phase shift of our waves. As such, the red line corresponds to the changes in
                the period - altered by the red buttons. The blue line corresponds to changes in the amplitude - altered by the blue buttons. Lastly, the grey line corresponds
                to changes in the phase shift - altered by the grey buttons. We can also find there is a scale for the period and amplitude located along both the sine and
                cosine axes. 

Principle 2:    Mappings is the second Norman principle that has guided our redesign. With the wave equation being moved to the centre of the app page, there are coloured buttons
                located above and below the equation. These buttons will alter the magnitude of the attribute that is vertically adjacent to it. For instance, the red arrows will 
                adjust the period of the wave, as it is vertically adjacent to the period variable in the wave equation.

Principle 3:    Discoverability is the third Norman principle that has guided our redesign. We've moved the wave equation along with the buttons to alter the wave attributes
                to the centre of the screen so that the user will notice the actionable objects of the app first. We've also added an instruction guide to the centre-bottom of 
                the screen so that it will also be one of the first things that the user will notice and read before attempting to use the app.
-}

module SinCreator exposing (..)

{-
Copyright 2017-2019 Christopher Kumar Anand,  Adele Olejarz, Chinmay Sheth, Yaminah Qureshi, Graeme Crawley and students of McMaster University.  Based on the Shape Creator by Levin Noronha.

   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution, and cite the paper

   @article{d_Alves_2018,
   title={Using Elm to Introduce Algebraic Thinking to K-8 Students},
   volume={270},
   ISSN={2075-2180},
   url={http://dx.doi.org/10.4204/EPTCS.270.2},
   DOI={10.4204/eptcs.270.2},
   journal={Electronic Proceedings in Theoretical Computer Science},
   publisher={Open Publishing Association},
   author={d’ Alves, Curtis and Bouman, Tanya and Schankula, Christopher and Hogg, Jenell and Noronha, Levin and Horsman, Emily and Siddiqui, Rumsha and Anand, Christopher Kumar},
   year={2018},
   month={May},
   pages={18–36}
   }

   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR AN, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)


init =
    { time = Nothing
    , currentTime = 0
    , notify = NotifyTap
    , uArg = 0
    , vArg = 0
    , editableArg = 0
    , uDilation = 1
    , vDilation = 1
    , editableDilation = 0
    , editableShift = 0
    , uScale = 20
    , vScale = 20
    , editableScale = 0
    , uShift = 0
    , uShiftScale = 1
    , u = 1
    , v = 1
    , rScale = 1
    , gScale = 1
    , bScale = 1
    , rFun = OneFun
    , bFun = UFun
    , gFun = VFun
    , sinGraph = []
    , cosGraph = []
    , defSinGraph = []
    , defCosGraph = []
    , vTransparency = 0.5
    , trigCycleU = Sin
    , trigCycleV = Sin
    , latestPointV = ( 0, 0, rgb 160 128 96 )
    , uTransform = ScaleU
    , moveX = ZeroFun
    , moveY = UFunZero
    , moveX1 = UFunZero
    , moveY1 = ZeroFun
    , transformFun = ZeroFun
    , uCosGraph = 0
    , uSinGraph = 0
    , uDefCosGraph = 0
    , uDefSinGraph = 0
    , editableYSinForTransforms = 0
    , r = 0
    , g = 0
    , b = 0
    , currentButton = None
    , buttonDownTime = 0
    , transformsRightArrowTransp = 0.25
    , transformsLeftArrowTransp = 0.25

    --, transformsNumTransp = 0.25
    , moveTextX = 0.25
    , moveTextY = 0.25
    , moveTextX1 = 0.25
    , moveTextY1 = 0.25
    , rTransp = 0.25
    , gTransp = 0.25
    , bTransp = 0.25
    , addAnotherFuncTransp = 0.25
    , uTextTransp = 0.5
    , vTextTransp = 0.5
    , maxAmplitude = 40
    , maxFrequency = 4
    , maxShift = 7
    , cosWaveLength = 200
    , sinWaveLength = 350
    }


type Msg m
    = Tick Float GetKeyState
    | TransM (m -> m)
    | Notif Notifications
    | R
    | G
    | B
    | UScalePlus
    | UDilationPlus
    | UShiftPlus
    | UScaleMinus
    | UDilationMinus
    | UShiftMinus
    | EditableScalePlus
    | EditableDilationPlus
    | EditableScaleMinus
    | EditableDilationMinus
    | VScalePlus
    | VScaleMinus
    | VDilationPlus
    | VDilationMinus
    | TrigCycleU
    | TrigCycleV
    | RScalePlus
    | RScaleMinus
    | GScalePlus
    | GScaleMinus
    | BScalePlus
    | BScaleMinus
    | ButtonDown ButtonDir
    | MouseUp


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


type FunType
    = OneFun
    | UFun
    | VFun


type Trig
    = Sin
    | Cos


type ZeroFunType
    = ZeroFun
    | UFunZero
    | NegUFun
    | VFunZero
    | NegVFun


type Transforms
    = ScaleU
    | MoveX
    | MoveY
    | MoveCircle
    | URotate
    | ScaleX
    | ScaleY
    | MakeTransparent
    | EditableXSin


type ButtonDir
    = AmplitudeUp
    | AmplitudeDown
    | FrequencyUp
    | FrequencyDown
    | ShiftUp
    | ShiftDown
    | EditableAmplitudeUp
    | EditableAmplitudeDown
    | EditableFrequencyUp
    | EditableFrequencyDown
    | RedUp
    | RedDown
    | BlueUp
    | BlueDown
    | GreenUp
    | GreenDown
    | None
    | VUP
    | VDown


update msg model =
    case msg of
        Tick t _ ->
            let
                uArg =
                    model.uArg + model.uDilation * (t - (t - 0.05))

                vArg =
                    model.vArg + model.vDilation * (t - (t - 0.05))

                editableArg =
                    model.editableArg + model.editableDilation * (t - (t - 0.05))

                currentTime =
                    case model.time of
                        Nothing ->
                            0

                        Just ct ->
                            ct

                u =
                    model.uScale * evalTrig model.trigCycleU uArg

                v =
                    model.vScale * evalTrig model.trigCycleV vArg

                r =
                    clamp 0 255 (abs (model.rScale * eval model.rFun u v))

                g =
                    clamp 0 255 (abs (model.gScale * eval model.gFun u v))

                b =
                    clamp 0 255 (abs (model.bScale * eval model.bFun u v))

                uSinGraph =
                    model.uScale * sin uArg

                sinGraphPoint =
                    ( 0, uSinGraph, rgb r g b )
                
                uDefSinGraph =
                    20 * sin uArg

                defSinGraphPoint =
                    ( 0, uDefSinGraph, rgb r g b )

                cosGraphPoint =
                    ( uCosGraph, 0, rgb r g b )

                uCosGraph =
                    model.uScale * cos uArg  
                
                uDefCosGraph =
                    20 * cos uArg

                defCosGraphPoint =
                    ( uDefCosGraph, 0, rgb r g b )

                editableYSinForTransforms =
                    model.editableScale * cos editableArg
            in
            { model
                | time = Just t
                , uArg = uArg
                , vArg = vArg
                , currentTime = currentTime
                , u = u
                , v = v
                , sinGraph =
                    List.take 2470
                        ([ sinGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    if xx >= model.sinWaveLength then
                                        Nothing

                                    else
                                        Just ( xx + 0.35, yy, cc )
                                )
                                model.sinGraph
                        )
                , defSinGraph =
                    List.take 2470
                        ([ defSinGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    if xx >= 350 then
                                        Nothing

                                    else
                                        Just ( xx + 0.35, yy, cc )
                                )
                                model.defSinGraph
                        )
                , cosGraph =
                    List.take 2470
                        ([ cosGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    -- Subtract 130 to account for the ratio of the screen and remove excess
                                    if yy <= -model.cosWaveLength then
                                        Nothing

                                    else
                                        Just ( xx, yy - 0.35, cc )
                                )
                                model.cosGraph
                        )
                , defCosGraph =
                    List.take 2470
                        ([ defCosGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    -- Subtract 130 to account for the ratio of the screen and remove excess
                                    if yy <= -200 then
                                        Nothing

                                    else
                                        Just ( xx, yy - 0.35, cc )
                                )
                                model.defCosGraph
                        )
                , r = r
                , g = g
                , b = b
                , uCosGraph = uCosGraph
                , uSinGraph = uSinGraph
                , uDefCosGraph = uDefCosGraph
                , uDefSinGraph = uDefSinGraph

                --, editableYSinForTransforms = editableYSinForTransforms
                , buttonDownTime =
                    case model.currentButton of
                        None ->
                            0

                        _ ->
                            model.buttonDownTime + 0.1
                , uScale =
                    case model.currentButton of
                        AmplitudeUp ->
                            if model.uScale < model.maxAmplitude then
                                model.uScale + curveX model.buttonDownTime

                            else if model.uScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.uScale

                        AmplitudeDown ->
                            if model.uScale > -model.maxAmplitude then
                                model.uScale - curveX model.buttonDownTime

                            else if model.uScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.uScale

                        _ ->
                            model.uScale
                , uDilation =
                    case model.currentButton of
                        FrequencyUp ->
                            if model.uDilation < model.maxFrequency then
                                model.uDilation + curveX model.buttonDownTime

                            else if model.uDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.uDilation

                        FrequencyDown ->
                            if model.uDilation > -model.maxFrequency then
                                model.uDilation - curveX model.buttonDownTime

                            else if model.uDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.uDilation

                        _ ->
                            model.uDilation
                , uShift =
                    case model.currentButton of
                        ShiftUp ->
                            if model.uShift < model.maxShift then
                                model.uShift + curveX model.buttonDownTime
                            else if model.uShift > model.maxShift then
                                model.maxShift
                            else
                                model.uShift
                        ShiftDown ->
                            if model.uShift > -model.maxShift then
                                model.uShift - curveX model.buttonDownTime
                            else if model.uShift < -model.maxShift then
                                -model.maxShift
                            else
                                model.uShift
                        _ ->
                            model.uShift
                , editableScale =
                    case model.currentButton of
                        EditableAmplitudeUp ->
                            if model.editableScale < model.maxAmplitude then
                                model.editableScale + curveX model.buttonDownTime

                            else if model.editableScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.editableScale

                        EditableAmplitudeDown ->
                            if model.editableScale > -model.maxAmplitude then
                                model.editableScale - curveX model.buttonDownTime

                            else if model.editableScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.editableScale

                        _ ->
                            model.editableScale
                , editableDilation =
                    case model.currentButton of
                        EditableFrequencyUp ->
                            if model.editableDilation < model.maxFrequency then
                                model.editableDilation + curveX model.buttonDownTime

                            else if model.editableDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.editableDilation

                        EditableFrequencyDown ->
                            if model.editableDilation > -model.maxFrequency then
                                model.editableDilation - curveX model.buttonDownTime

                            else if model.editableDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.editableDilation

                        _ ->
                            model.editableDilation
                , editableShift =
                    case model.currentButton of
                        ShiftUp ->
                            model.editableShift + curveX model.buttonDownTime

                        ShiftDown ->
                            model.editableShift - curveX model.buttonDownTime

                        _ ->
                            model.editableShift
                , rScale =
                    case model.currentButton of
                        RedUp ->
                            if model.rScale < 253 then
                                model.rScale + curveX model.buttonDownTime

                            else
                                model.rScale

                        RedDown ->
                            if model.rScale > 2 then
                                model.rScale - curveX model.buttonDownTime

                            else
                                model.rScale

                        _ ->
                            model.rScale
                , bScale =
                    case model.currentButton of
                        BlueUp ->
                            if model.bScale < 253 then
                                model.bScale + curveX model.buttonDownTime

                            else
                                model.bScale

                        BlueDown ->
                            if model.bScale > 2 then
                                model.bScale - curveX model.buttonDownTime

                            else
                                model.bScale

                        _ ->
                            model.bScale
                , gScale =
                    case model.currentButton of
                        GreenUp ->
                            if model.gScale < 252 then
                                model.gScale + curveX model.buttonDownTime

                            else
                                model.gScale

                        GreenDown ->
                            if model.gScale > 2 then
                                model.gScale - curveX model.buttonDownTime

                            else
                                model.gScale

                        _ ->
                            model.gScale
                , vScale =
                    case model.currentButton of
                        VUP ->
                            if model.vScale < 48 then
                                model.vScale + curveX model.buttonDownTime

                            else
                                model.vScale

                        VDown ->
                            if model.vScale > -48 then
                                model.vScale - curveX model.buttonDownTime

                            else
                                model.vScale

                        _ ->
                            model.vScale
            }

        TransM t ->
            t model

        -- ran out of room for notifications, but left them here for a possible future improvement
        Notif notif ->
            { model | notify = notif }

        R ->
            { model | rFun = cycleFun model.rFun }

        G ->
            { model | gFun = cycleFun model.gFun }

        B ->
            { model | bFun = cycleFun model.bFun }

        RScalePlus ->
            { model
                | rScale =
                    if model.rScale < 255 then
                        model.rScale + 1

                    else
                        model.rScale
            }

        RScaleMinus ->
            { model
                | rScale =
                    if model.rScale > 0 then
                        model.rScale - 1

                    else
                        model.rScale
            }

        GScalePlus ->
            { model
                | gScale =
                    if model.gScale < 255 then
                        model.gScale + 1

                    else
                        model.gScale
            }

        GScaleMinus ->
            { model
                | gScale =
                    if model.gScale > 0 then
                        model.gScale - 1

                    else
                        model.gScale
            }

        BScalePlus ->
            { model
                | bScale =
                    if model.bScale < 255 then
                        model.bScale + 1

                    else
                        model.bScale
            }

        BScaleMinus ->
            { model
                | bScale =
                    if model.bScale > 0 then
                        model.bScale - 1

                    else
                        model.bScale
            }

        UScalePlus ->
            { model
                | uScale =
                    if model.uScale < model.maxAmplitude then
                        model.uScale + 10

                    else
                        model.uScale
            }

        UScaleMinus ->
            { model
                | uScale =
                    if model.uScale > -model.maxAmplitude then
                        model.uScale - 10

                    else
                        model.uScale
            }

        UDilationPlus ->
            { model
                | uDilation =
                    if model.uDilation < model.maxFrequency then
                        model.uDilation + 0.25

                    else
                        model.uDilation
            }

        UDilationMinus ->
            { model
                | uDilation =
                    if model.uDilation > 0 then
                        model.uDilation - 0.25

                    else
                        model.uDilation
            }

        UShiftPlus ->
            { model
                | uArg =
                    model.uArg + model.uShiftScale * Basics.pi / 4
                , uShift = model.uShift + model.uShiftScale
            }

        UShiftMinus ->
            { model
                | uArg =
                    model.uArg - model.uShiftScale * Basics.pi / 4
                , uShift = model.uShift - model.uShiftScale
            }

        EditableScalePlus ->
            { model
                | editableScale =
                    if model.editableScale < model.maxAmplitude then
                        model.editableScale + 1

                    else
                        model.editableScale
            }

        EditableScaleMinus ->
            { model
                | editableScale =
                    if model.editableScale > -model.maxAmplitude then
                        model.editableScale - 1

                    else
                        model.editableScale
            }

        EditableDilationPlus ->
            { model
                | editableDilation =
                    if model.editableDilation < model.maxFrequency then
                        model.editableDilation + 1

                    else
                        model.editableDilation
            }

        EditableDilationMinus ->
            { model
                | editableDilation =
                    if model.editableDilation > -model.maxFrequency then
                        model.editableDilation - 1

                    else
                        model.editableDilation
            }

        VScalePlus ->
            { model
                | vScale =
                    if model.vScale < model.maxAmplitude then
                        model.vScale + 1

                    else
                        model.vScale
            }

        VScaleMinus ->
            { model
                | vScale =
                    if model.vScale > -model.maxAmplitude then
                        model.vScale - 1

                    else
                        model.vScale
            }

        VDilationPlus ->
            { model
                | vDilation =
                    if model.vDilation < model.maxFrequency then
                        model.vDilation + 1

                    else
                        model.vDilation
            }

        VDilationMinus ->
            { model | vDilation = model.vDilation - 1 }

        TrigCycleU ->
            { model | trigCycleU = cycleTrig model.trigCycleU }

        TrigCycleV ->
            { model | trigCycleV = cycleTrig model.trigCycleV }

        ButtonDown dir ->
            { model | currentButton = dir }

        MouseUp ->
            { model | currentButton = None }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads


eval f u v =
    case f of
        OneFun ->
            u

        UFun ->
            u

        VFun ->
            v


showFun f u v =
    case f of
        OneFun ->
            "u"

        UFun ->
            "u"

        VFun ->
            "v"


cycleFun f =
    case f of
        OneFun ->
            UFun

        UFun ->
            VFun

        VFun ->
            OneFun


cycleTrig f =
    case f of
        Sin ->
            Cos

        Cos ->
            Sin


textTrig f =
    case f of
        Sin ->
            "sin"

        Cos ->
            "cos"


evalTrig f u =
    case f of
        Sin ->
            sin u

        Cos ->
            cos u


cycleFunZero f =
    case f of
        ZeroFun ->
            UFunZero

        UFunZero ->
            NegUFun

        NegUFun ->
            VFunZero

        VFunZero ->
            NegVFun

        NegVFun ->
            ZeroFun


moveText mv =
    case mv of
        ZeroFun ->
            "u"

        UFunZero ->
            "u"

        NegUFun ->
            "-u"

        VFunZero ->
            "v"

        NegVFun ->
            "-v"

cycleTransforms tr =
    case tr of
        ScaleU ->
            URotate

        URotate ->
            ScaleX

        ScaleX ->
            ScaleY

        ScaleY ->
            MakeTransparent

        MakeTransparent ->
            MoveX

        MoveX ->
            MoveY

        MoveY ->
            MoveCircle

        MoveCircle ->
            EditableXSin

        EditableXSin ->
            ScaleU


-- change you app's state based on your new messages


numGraphPoints model =
    round 2505


curveX x =
    Basics.toFloat (round (clamp 0 12 (x ^ 2) / 4))


sinCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.sinGraph (List.drop 1 model.sinGraph)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)

defSinCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.defSinGraph (List.drop 1 model.defSinGraph)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (dotted 0.8) grey) points)

cosCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.cosGraph (List.drop 1 model.cosGraph)
    in
    List.take (numGraphPoints model - 1) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)

defCosCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.defCosGraph (List.drop 1 model.defCosGraph)
    in
    List.take (numGraphPoints model - 1) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (dotted 0.8) grey) points)

cosinString model =
    let
        fraction =
            if (model.uShift / 8 * 2) < 0 then
                showDigits 5 (model.uShift / 8 * 2)

            else
                "+" ++ showDigits 4 (model.uShift / 8 * 2)
    in
    showDigits 5 model.uDilation ++ "t" ++ fraction ++ "π)"


view model =
    let
        uScale =
            model.uScale

        u =
            model.u

        v =
            model.v

        uArg =
            model.uArg

        x1 =
            if model.uTransform == MakeTransparent then
                90

            else
                45

        notTrigCycleU =
            if model.trigCycleU == Sin then
                cos

            else
                sin

        tt str =
            str |> text |> serif |> italic |> size 10 |> filled titleColour

        x2 =
            if model.uTransform == MakeTransparent then
                116

            else
                81

        setofTriangles =
            group
                [ upArrow |> filled red |> notifyTap UDilationPlus |> move ( -55, -5 ) |> notifyMouseDown (ButtonDown FrequencyUp) |> notifyMouseUp (ButtonDown None)
                , upArrow |> filled blue |> notifyTap UScalePlus |> move ( -110, -5 ) |> notifyMouseDown (ButtonDown AmplitudeUp) |> notifyMouseUp (ButtonDown None)
                , upArrow |> filled grey |> notifyTap UShiftPlus |> move ( -15, -5 ) |> notifyMouseDown (ButtonDown ShiftUp) |> notifyMouseUp (ButtonDown None)
                , downArrow |> filled red |> notifyTap UDilationMinus |> move ( -55, -20 ) |> notifyMouseDown (ButtonDown FrequencyDown) |> notifyMouseUp (ButtonDown None)
                , downArrow |> filled blue |> notifyTap UScaleMinus |> move ( -110, -20 ) |> notifyMouseDown (ButtonDown AmplitudeDown) |> notifyMouseUp (ButtonDown None)
                , downArrow |> filled grey |> notifyTap UShiftMinus |> move ( -15, -20 ) |> notifyMouseDown (ButtonDown ShiftDown) |> notifyMouseUp (ButtonDown None)
                ]

        amplitudeLineSine =
            group
                [   line ( -50, 50 ) ( -50, abs model.uScale + 50) |> outlined (solid 1) blue
                    , triangle 2 |> filled blue |> rotate (degrees 90) |> move (-50, abs model.uScale + 50)
                    , triangle 2 |> filled blue |> rotate (degrees 270) |> move (-50, 50)
                    , text "amplitude" |> size 8 |> filled blue |> rotate (degrees -90) |> move (-45, 85)
                ]

        amplitudeLineCos =
            group
                [   line ( -50, 50 ) (abs model.uScale - 50,50) |> outlined (solid 1) blue
                    , triangle 2 |> filled blue |> rotate (degrees 0) |> move (abs model.uScale - 50, 50)
                    , triangle 2 |> filled blue |> rotate (degrees 180) |> move (-50, 50)
                    , text "amplitude" |> size 8 |> filled blue |> move (-45, 40)
                ]
        
        periodLineSine =
            group
                [   line ((-((2*Basics.pi)/(abs model.uDilation)) + 24)*4, 100 ) (((2*Basics.pi)/(abs model.uDilation) + 23)*4,100) |> outlined (solid 1) red
                    , triangle 2 |> filled red |> rotate (degrees 0) |> move (((2*Basics.pi)/(abs model.uDilation) + 23)*4 - 1, 100)
                    , triangle 2 |> filled red |> rotate (degrees 180) |> move ((-((2*Basics.pi)/(abs model.uDilation)) + 24)*4 + 1, 100)
                    , text "period" |> size 8 |> filled red |> move (85, 102)
                ]

        periodLineCos =
            group
                [   line (-100,(-((2*Basics.pi)/(abs model.uDilation)) + 5)*4) (-100 ,((2*Basics.pi)/(abs model.uDilation) + 4)*4) |> outlined (solid 1) red
                    , triangle 2 |> filled red |> rotate (degrees 90) |> move (-100,((2*Basics.pi)/(abs model.uDilation) + 4)*4 - 1)
                    , triangle 2 |> filled red |> rotate (degrees 270) |> move (-100,(-((2*Basics.pi)/(abs model.uDilation)) + 5)*4 + 1)
                    , text "period" |> size 8 |> filled red |> rotate (degrees 90) |> move (-103, 8)
                ]
        
        baseEquation = 
            group
                [
                    text "y =" |> fixedwidth |> italic |> filled black |> move ( -120, 0 )
                    , text "A" |> fixedwidth |> italic |> filled blue |> move ( -95, 0 )
                    , text "sin(" |> fixedwidth |> italic |> filled black |> move ( -80, 0 )
                    , text "B" |> fixedwidth |> italic |> filled red |> move ( -50, 0 )
                    , text "(x +" |> fixedwidth |> italic |> filled black |> move ( -40, 0 )
                    , text "C" |> fixedwidth |> italic |> filled grey |> move ( -10, 0 )
                    , text "))" |> fixedwidth |> italic |> filled black |> move ( -2, 0 )
                ]

        -- Circle that rotates in time with the sin & cosin waves
        circleGraphics =
            group
                [ line ( -50, 50 ) ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) |> outlined (solid 1) red
                , line ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) ( 0, 50 + model.uSinGraph ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.5
                , line ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) ( model.uCosGraph - 50, 0 ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.5
                , line ( -50, 50 ) ( (model.uScale * notTrigCycleU uArg) - 50, 50 ) |> outlined (solid 1) blue
                , line ((model.uScale * notTrigCycleU uArg) - 50, 50 ) (-50 + model.uScale * notTrigCycleU uArg, 50 + u) |> outlined (solid 1) yellow
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( 0, 50 + model.uSinGraph )
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( model.uCosGraph - 50, 0 )
                , circle (abs uScale) |> outlined (solid 1) black |> move ( -50, 50 )
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( -50 + model.uScale * notTrigCycleU uArg, 50 + u )
                , circle 5 |> outlined (dotted 1) grey |> move ( -50, 50 )
                ]

        graphScale =
            group   
                [ line (0,0) (10,0) |> outlined (solid 0.5) black
                , line (0,0) (0,-10) |> outlined (solid 0.5) black
                , triangle 2 |> filled black |> rotate (degrees 0) |> move (8,0)
                , triangle 2 |> filled black |> rotate (degrees 270) |> move (0,-8)
                , text "Scale" |> size 6 |> filled black |> move (-1,3)   
                , text "1" |> size 5 |> filled black |> move (12,-2)
                , text "1" |> size 5 |> filled black |> move (-1,-16) 
                ]

        instructions =
            group   
                [ line (-50, -23) (260, -23) |> outlined (solid 0.5) black
                , line (-50, -87) (260, -87) |> outlined (solid 0.5) black
                , line (-50, -23) (-50, -87) |> outlined (solid 0.5) black
                , line (260, -23) (260, -87) |> outlined (solid 0.5) black
                , text "How to Use SinCreator" |> size 15  |> filled orange |> move (-40,-40)
                , text "SinCreator simulates sine waves on the horizontal axis & cosine waves on the vertical axis." |> size 8 |> filled black |> move (-40,-50)
                , text "1. Use the" |> size 7 |> filled black |> move (-40,-60)
                , text "BLUE ARROWS" |> size 7 |> filled blue |> move (-8,-60)
                , text "to adjust the amplitude." |> size 7 |> filled black |> move (47,-60)
                , text "2. Use the" |> size 7 |> filled black |> move (-40,-70)
                , text "RED ARROWS" |> size 7 |> filled red |> move (-8,-70)
                , text "to adjust the period." |> size 7 |> filled black |> move (42,-70)
                , text "3. Use the" |> size 7 |> filled black |> move (-40,-80)
                , text "GREY ARROWS" |> size 7 |> filled grey |> move (-8,-80)
                , text "to adjust the phase shift." |> size 7 |> filled black |> move (47,-80)
                ]

        cosLabel =
            text (String.fromFloat (model.uScale/10) ++ "cos(" ++ cosinString model) |> fixedwidth |> size 8 |> filled black |> move ( -110, -82 ) |> notifyTap (TransM (\m -> { m | trigCycleU = Cos }))
    in
    [ graphPaperCustom 10 1 (rgb 255 137 5) |> makeTransparent 0.25 -- axes and selected coordinate ticks
    , group
        [ rect 1000 0.5 |> filled brown
        , rect 0.5 1000 |> filled brown
        , group (sinCurve model) |> move ( 0, 50 )
        , group (cosCurve model) |> move ( -50, 0 )
        , group (defSinCurve model) |> move ( 0, 50 )
        , group (defCosCurve model) |> move ( -50, 0 )
        , trigGraphAxis model |> move ( -185, 70 )
        , circleGraphics
        , amplitudeLineSine |> move (405,0)
        , amplitudeLineCos |> move (0,-252)
        , periodLineSine |> move (10,0)
        , periodLineCos |> move (0,-120)
        , graphScale |> move (-100,90)
        , instructions |> move (75, -150)
        ]
        |> move ( -140, 80 )
    , cosLabel |> move ( -137, -65 )
    , baseEquation |> scale 1.5 |> move (110,20)
    , group
        [ functionText model |> scale 1.5 |> move ( 70, -60 )
        , setofTriangles |> scale 1.5 |> move ( 70, -39 )
        ]
        |> move ( 50, 15 )
    ]


upArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ]


downArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] 


trigGraphAxis model =
    group
        [ rect 0.5 105 |> filled black |> move ( 185, -18 )
        , rect model.sinWaveLength 0.5 |> filled black |> move ( 185 + model.sinWaveLength / 2, -20 )

        -- Subtract 130 to account for the ratio of the screen and remove excess
        , rect 105 0.5 |> filled black |> move ( 132, -70 )
        , rect 0.5 model.cosWaveLength |> filled black |> move ( 135, -70 - model.cosWaveLength / 2 )
        ]


functionText model =
    group
        [ text (showDigits 2 (model.uScale/10) ++ textTrig model.trigCycleU ++ "(" ++ cosinString model) |> fixedwidth |> size 10 |> filled black |> move ( -120, 0 )
        ]


showDigits width x =
    "      " ++ String.fromFloat x |> String.right width


titleColour =
    rgba 200 0 0 0.95


copiable str =
    str |> text |> selectable |> fixedwidth |> size 6 |> filled black


copiable2 str =
    str |> text |> selectable |> fixedwidth |> size 9 |> filled black
