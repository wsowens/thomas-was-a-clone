module Main exposing (..)

import Browser
import Browser.Events as Events
import Svg exposing (..)
import Svg.Attributes as Attr
import Json.Decode as Decode

-- MAIN
main = 
    Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Vector =
    { x : Float
    , y : Float
    }

type alias Square =
    { pos : Vector
    , vel : Vector
    , size : Vector
    }

zeroVector = { x = 0, y = 0}

type alias Model =
    { square : Square
    , width : Float
    , height : Float
    }

squareSize = { x = 100, y = 100 }

-- INIT
init : (Float, Float) -> (Model, Cmd Msg)
init (width, height) =
    ({ width = width
      , height = height
      , square = 
        { pos = { x = width / 2 - squareSize.x / 2, y = 0}
        , vel = zeroVector
        , size = squareSize
        }
    }
    , Cmd.none
    )

-- UPDATE
type Msg
    = KeyDown KeyType
    | KeyUp KeyType
    | Resize (Int, Int)
    | Tick Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Resize (width, height) ->
            ( boundsCheck { model | width = (toFloat width) * 0.95, height = (toFloat height) * 0.95 }
            , Cmd.none
            )
        Tick d ->
            ( boundsCheck { model | square = (timeStep model.square d) }
            , Cmd.none
            )
        _ ->
            ( boundsCheck model
            , Cmd.none
            )

boundsCheck : Model -> Model
boundsCheck model =
    let 
        pos = model.square.pos
        vel = model.square.vel
        size = model.square.size
        (x, velx) = checker 0 (model.width  - size.x) pos.x vel.x
        (y, vely) = checker 0 (model.height - size.y) pos.y vel.y
        square = { size = model.square.size, pos = {x = x, y = y}, vel = { x = velx, y = vely} }
    in
    { model | square = square }

checker: Float -> Float -> Float -> Float -> (Float, Float)
checker lower upper x vel =
    if x < lower then
        (lower, 0)
    else if x > upper then
        (upper, 0)
    else
        (x, vel)

gravity = 0.005
friction = 0.1
timeStep : Square -> Float -> Square
timeStep square delta =
    let
        posx = square.pos.x + square.vel.x * delta
        posy = square.pos.y + square.vel.y * delta + gravity * delta * delta * 0.5
        decay_factor = delta * friction
        vely = square.vel.y + gravity * delta
        velx = 
            -- on the ground
            if (abs square.vel.y) < 0.01 then
                if square.vel.x < 0 then
                    if square.vel.x > -1 * decay_factor then
                        0
                    else
                        square.vel.x + decay_factor
                else
                    if square.vel.x < decay_factor then
                        0
                    else
                        square.vel.x - decay_factor
            else
                square.vel.x
        

    in
    { square | pos = {x = posx, y = posy }, vel = {x = velx, y = vely} }

-- VIEW
view: Model -> Browser.Document Msg
view model =
    let 
        x = String.fromFloat model.square.pos.x
        y = String.fromFloat model.square.pos.y
        squareX = String.fromFloat model.square.size.x
        squareY = String.fromFloat model.square.size.y
        width = String.fromFloat model.width
        height = String.fromFloat model.height
    in
    { title = "Square Adventures"
    , body = [
        svg
            [ Attr.width width
            , Attr.height height
            , Attr.style "border: 1px solid black; margin: auto 2%"
            ]
            [
                Svg.rect [ Attr.width squareX, Attr.height squareY, Attr.x x, Attr.y y] []
            ]
    ]
    }

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Events.onResize (\w -> \h -> Resize (w, h))
    , Events.onKeyDown (Decode.map KeyDown keyDecoder)
    , Events.onKeyUp (Decode.map KeyUp keyDecoder)
    , Events.onAnimationFrameDelta (\tick -> Tick tick)
    ]
    

-- Directions
type KeyType
    = Left
    | Right
    | Up
    | Down
    | Other

keyDecoder : Decode.Decoder KeyType
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> KeyType
toDirection string =
    case string of
        "A" ->
            Left
        "a" ->
            Left
        "ArrowLeft" ->
            Left
        "d" ->
            Right
        "D" ->
            Right
        "ArrowRight" ->
            Right
        "W" ->
            Up
        "w" ->
            Up
        "ArrowUp" ->
            Up
        "S" ->
            Down
        "s" ->
            Down
        "ArrowDown" ->
            Down
        _ ->
            Other

keyToBumpX : KeyType -> Float
keyToBumpX k =
    case k of
        Left -> -1
        Right -> 1
        _ -> 0

keyToBumpY : KeyType -> Float
keyToBumpY k =
    case k of
        Up -> -2
        Down -> 1
        _ -> 0
