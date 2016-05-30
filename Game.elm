module Game exposing (..)

import Html exposing (Html)
import Html.App as Html
import Svg exposing (Svg,svg,rect,circle,text',text)
import Svg.Attributes as SvgA exposing (version,x,y,viewBox,width,height,fill,cx,cy,r,style)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time, second)
import Key exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- CONSTANTS

(playgroundWidth,playgroundHeight) = (600, 400)
(halfWidth,halfHeight) = (playgroundWidth/2, playgroundHeight/2)

(paddleWidth,paddleHeight) = (10, playgroundHeight / 10)
paddlePosMin = paddleHeight / 2
paddlePosMax = playgroundHeight - paddleHeight / 2

paddleMargin = 10

ballRadius = 7

paddleVelocity = 0.5
ballVelocity = 0.2

backgroundColor = "green"

paddleColor = "white"
ballColor = "white"
textColor = "white"

-- MODEL
type alias Paddle =
  { x : Float,
    y : Float,
    dy : Float
  }

type alias Ball =
  { x : Float,
    y : Float,
    dx : Float,
    dy : Float
  }

startBall =  {x = halfWidth, y = halfHeight, dx = ballVelocity, dy = ballVelocity}

type Mode
  = Play
  | Pause
  | Restart Int

type alias Model =
    { mode : Mode,
      leftPaddle : Paddle,
      rightPaddle : Paddle,
      ball : Ball,
      leftScore : Int,
      rightScore : Int
    }

type Side
    = Left | Right

model : Model
model =
    { mode = Pause,
      leftPaddle = { x = paddleMargin,  y = halfHeight, dy = 0},
      rightPaddle = { x = playgroundWidth - paddleMargin, y = halfHeight, dy = 0},
      ball = startBall,
      leftScore = 0,
      rightScore = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | CountDown Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( applyPhysics dt model, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )

        CountDown time ->
            ( countDown model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        RightPaddleUp ->
            updatePaddleVelocity Right -paddleVelocity model

        RightPaddleDown ->
            updatePaddleVelocity Right paddleVelocity model

        LeftPaddleUp ->
            updatePaddleVelocity Left -paddleVelocity model

        LeftPaddleDown ->
            updatePaddleVelocity Left paddleVelocity model

        StartPause ->
            toggleStartPause model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        RightPaddleUp ->
            updatePaddleVelocity Right 0.0 model

        RightPaddleDown ->
            updatePaddleVelocity Right 0.0 model

        LeftPaddleUp ->
            updatePaddleVelocity Left 0.0 model

        LeftPaddleDown ->
            updatePaddleVelocity Left 0.0 model

        _ ->
            model

toggleStartPause : Model -> Model
toggleStartPause model =
    case model.mode of
      Play ->
        {model | mode = Pause}

      Restart time ->
        {model | mode = Pause}

      Pause ->
        {model | mode = Restart 3}

countDown : Model -> Model
countDown model =
    case model.mode of
      Restart time ->
        if time == 1 then
          {model | mode = Play}
        else
          {model | mode = Restart (time - 1)}
      Play ->
        model

      Pause ->
        model


applyPhysics : Float -> Model -> Model
applyPhysics dt ({leftPaddle, rightPaddle, ball} as model) =
    let
      newModel =
        { model |
          leftPaddle = applyPaddlePhysics dt leftPaddle,
          rightPaddle = applyPaddlePhysics dt rightPaddle,
          ball = applyBallPhysics dt ball
        }
    in
      updateDirectionAndScore newModel


applyPaddlePhysics : Float -> Paddle -> Paddle
applyPaddlePhysics dt paddle =
    let
      newY =
        paddle.y + paddle.dy * dt
      boundedY =
        min paddlePosMax (max paddlePosMin newY)
    in
      { paddle | y = boundedY }


applyBallPhysics : Float -> Ball -> Ball
applyBallPhysics dt ball =
    let
      newX =
        ball.x + ball.dx * dt

      newY =
        ball.y + ball.dy * dt
    in
      { ball | x = newX, y = newY }


hitTheUpperOrLowerWall : Float -> Float -> Bool
hitTheUpperOrLowerWall y dy =
  if y < ballRadius && dy < 0 then
    True
  else if y > playgroundHeight-ballRadius && dy > 0 then
    True
  else
    False


hitThePaddle : Ball -> Paddle -> Bool
hitThePaddle ball paddle =
  let
    ballBounds = bounds ball.x ball.y ballRadius ballRadius
    paddleBounds = bounds paddle.x paddle.y paddleWidth paddleHeight
  in
    intersect ballBounds paddleBounds


bounds : Float -> Float -> Float -> Float -> (Float,Float,Float,Float)
bounds x y width height =
  let
    halfWidth = width / 2
    halfHeight = height / 2
  in
    (x- halfWidth, y - halfHeight, x + halfWidth, y + halfHeight)


intersect : (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> Bool
intersect (x1,y1,x2,y2) (x3,y3,x4,y4) =
  not (x2 < x3 || x4 < x1 || y2 < y3 || y4 < y1)


updatePaddleVelocity : Side -> Float -> Model -> Model
updatePaddleVelocity side dy ({leftPaddle, rightPaddle} as model) =
  case side of
    Left ->
      { model | leftPaddle = {leftPaddle | dy = dy} }

    Right ->
      { model | rightPaddle = {rightPaddle | dy = dy} }


updateDirectionAndScore : Model -> Model
updateDirectionAndScore ({ball, leftPaddle, rightPaddle, mode} as model)  =
    let
      hitLeftPaddle =
        ball.dx<0 && hitThePaddle ball leftPaddle

      hitRightPaddle =
        ball.dx>0 && hitThePaddle ball rightPaddle

      leftMissedBall =
        (not hitLeftPaddle) && ball.dx<0 && ball.x < ballRadius + paddleMargin

      rightMissedBall =
        (not hitRightPaddle) && ball.dx>0 && ball.x > playgroundWidth - ballRadius - paddleMargin

      newDx =
        if hitLeftPaddle || hitRightPaddle then
          -ball.dx
        else
          ball.dx

      newDy =
        if hitTheUpperOrLowerWall ball.y ball.dy then
          -ball.dy
        else
          ball.dy

      newBall =
        if leftMissedBall then
          startBall
        else if rightMissedBall then
          { startBall | dx = -startBall.dx}
        else
          { ball | dx = newDx, dy = newDy }

      newMode =
        if leftMissedBall || rightMissedBall then
          Restart 3
        else
          mode

      newLeftScore =
        if rightMissedBall then
          model.leftScore + 1
        else
          model.leftScore

      newRightScore =
        if leftMissedBall then
          model.rightScore + 1
        else
          model.rightScore
    in
      { model | ball = newBall, leftScore = newLeftScore, rightScore = newRightScore, mode = newMode }


-- VIEW


view : Model -> Html msg
view model =
    svg
      [version "1.1", x "0", y "0", width "100%", height "100%", viewBox ("0 0 " ++ (toString playgroundWidth) ++ " " ++ (toString playgroundHeight))]
      [rect
        [x "0", y "0", width (toString playgroundWidth), height (toString playgroundHeight), fill backgroundColor] [],
       renderPaddle model.leftPaddle,
       renderPaddle model.rightPaddle,
       renderBall model.ball,
       renderScore model.leftScore model.rightScore,
       renderMode model.mode ]


renderPaddle : Paddle -> Svg msg
renderPaddle paddle =
    let
      rx = paddle.x - paddleWidth / 2
      ry = paddle.y - paddleHeight / 2
    in
      rect [x (toString rx), y (toString ry), width (toString paddleWidth), height (toString paddleHeight), fill paddleColor] []


renderBall : Ball -> Svg msg
renderBall ball =
    circle [cx (toString ball.x), cy (toString ball.y), r (toString ballRadius), fill ballColor] []


renderScore : Int -> Int -> Svg msg
renderScore left right =
    text' [x (toString halfWidth), y "20", fill textColor, style "font-size:20px; text-anchor: middle"] [text ("Score " ++ (toString left) ++" : " ++ (toString right))]


renderMode : Mode -> Svg msg
renderMode mode =
  case mode of
    Play ->
      text' [x (toString halfWidth), y (toString (playgroundHeight-20)), fill textColor, style "font-size:10px; text-anchor: middle"] [text ("Press Space to Pause")]

    Pause ->
      text' [x (toString halfWidth), y (toString (halfHeight-40)), fill textColor, style "font-size:20px; text-anchor: middle"] [text ("Press Space to Play")]

    Restart time ->
        text' [x (toString halfWidth), y (toString (halfHeight-40)), fill textColor, style "font-size:40px; text-anchor: middle"] [text (toString time)]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
      Play ->
        Sub.batch
            [ AnimationFrame.diffs TimeUpdate
            , Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            ]

      Pause ->
        Sub.batch
            [ Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            ]

      Restart time ->
          Sub.batch
            [ Time.every second CountDown
            , Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            ]
