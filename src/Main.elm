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

zeroVector = { x = 0, y = 0}

type alias Model =
    { position : Vector
    , velocity : Vector
    , width : Float
    , height : Float
    }

-- INIT
init : (Float, Float) -> (Model, Cmd Msg)
init (width, height) =
    ( { position = zeroVector, velocity = zeroVector, width = width, height = height }
    , Cmd.none
    )

-- UPDATE
type Msg
    = Key KeyType
    | Resize (Int, Int)
    | Tick Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key k ->
            ( boundsCheck { model | velocity = updateVector model k }
            , Cmd.none
            )
        Resize (width, height) ->
            ( boundsCheck { model | width = (toFloat width) * 0.95, height = (toFloat height) * 0.95 }
            , Cmd.none
            )
        Tick d ->
            ( boundsCheck (timeStep model d)
            , Cmd.none
            )

updateVector : Model -> KeyType -> Vector
updateVector model k =
    let
        vel = model.velocity 
        bumpX = keyToBumpX k
        bumpY = keyToBumpY k
    in
    { x = vel.x + bumpX, y = vel.y + bumpY } 

boundsCheck : Model -> Model
boundsCheck model =
    let 
        pos = model.position
        vel = model.velocity
        (x, velx) = checker 0 (model.width  - 100) pos.x vel.x
        (y, vely) = checker 0 (model.height - 100) pos.y vel.y
    in
    { model | position = {x = x, y = y}, velocity = { x = velx, y = vely} }

checker: Float -> Float -> Float -> Float -> (Float, Float)
checker lower upper x vel =
    if x < lower then
        (lower, 0)
    else if x > upper then
        (upper, 0)
    else
        (x, vel)

gravity = 0.01
friction = 0.1
timeStep : Model -> Float -> Model
timeStep model delta =
    let
        posx = model.position.x + model.velocity.x * delta
        posy = model.position.y + model.velocity.y * delta + gravity * delta * delta * 0.5
        decay_factor = delta * friction
        vely = model.velocity.y + gravity * delta
        velx = 
            -- on the ground
            if model.velocity.y < 0.5 then
                if model.velocity.x < 0 then
                    if model.velocity.x > -1 * decay_factor then
                        0
                    else
                        model.velocity.x + decay_factor
                else
                    if model.velocity.x < decay_factor then
                        0
                    else
                        model.velocity.x - decay_factor
            else
                model.velocity.x
        

    in
    { model | position = {x = posx, y = posy }, velocity = {x = velx, y = vely} }

-- VIEW
view: Model -> Browser.Document Msg
view model =
    let 
        x = String.fromFloat model.position.x
        y = String.fromFloat model.position.y
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
                Svg.rect [ Attr.width "100", Attr.height "100", Attr.x x, Attr.y y] []
            ]
    ]
    }

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Events.onResize (\w -> \h -> Resize (w, h))
    , Events.onKeyDown (Decode.map Key keyDecoder)
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
        Left -> -2
        Right -> 2
        _ -> 0

keyToBumpY : KeyType -> Float
keyToBumpY k =
    case k of
        Up -> -2
        Down -> 1
        _ -> 0
