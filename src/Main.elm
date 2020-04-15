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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Key k ->
            ( boundsCheck { model | position = updateVector model k }
            , Cmd.none
            )
        Resize (width, height) ->
            ( boundsCheck { model | width = (toFloat width) * 0.95, height = (toFloat height) * 0.95 }
            , Cmd.none
            )

updateVector : Model -> KeyType -> Vector
updateVector model k =
    let
        pos = model.position 
        bumpX = keyToBumpX k
        bumpY = keyToBumpY k
    in
    { pos | x = pos.x + bumpX, y = pos.y + bumpY } 

boundsCheck : Model -> Model
boundsCheck model =
    let 
        coords = model.position
        x = checker 0 (model.width  - 100)  coords.x
        y = checker 0 (model.height - 100) coords.y
    in
    { model | position = {x = x, y = y}}

checker: Float -> Float -> Float -> Float
checker lower upper x =
    if x < lower then
        lower
    else if x > upper then
        upper
    else
        x
    

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
    let 
        _ = Debug.log "key: " string
    in
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
        Left -> -20
        Right -> 20
        _ -> 0

keyToBumpY : KeyType -> Float
keyToBumpY k =
    case k of
        Up -> -20
        Down -> 20
        _ -> 0
