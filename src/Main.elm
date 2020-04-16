module Main exposing (..)

import Browser
import Html
import Browser.Events as Events
import Svg
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

zeroVector = { x = 0, y = 0 }
defaultSize = { x = 50, y = 100 }

type State
    = Active
    | Inactive

type alias MoveInputs =
    { up : State
    , down : State
    , left : State
    , right : State
    }

start = { up = Inactive, down = Inactive, left = Inactive, right = Inactive}

type alias Model =
    { square : Square
    , width : Float
    , height : Float
    , inputs : MoveInputs
    }

-- INIT
init : (Float, Float) -> (Model, Cmd Msg)
init (width, height) =
    ({ width = width
      , height = height
      , square = 
        { pos = { x = width / 2 - defaultSize.x / 2, y = 0}
        , vel = zeroVector
        , size = defaultSize
        }
     , inputs = start
    }
    , Cmd.none
    )

-- UPDATE
type Msg
    = KeyEvent (KeyType, State)
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
            ( boundsCheck { model | square = (timeStep model.square model.inputs d) }
            , Cmd.none
            )
        KeyEvent (k, s) ->
            let
                inputs = handleKey (model.inputs) k s
                square = jump (inputs.up == Active && model.inputs.up == Inactive ) model.square
            in
            ( { model | inputs = inputs, square = square }
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
        square = { size = model.square.size, pos = {x = x, y = y}, vel = { x = velx, y = vely}}
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
maxspeed = 1.2
acceleration = 0.007
deceleration = 0.004
timeStep : Square -> MoveInputs -> Float -> Square
timeStep square inputs delta =
    let
        (velx, accx) =
            {-
                Handling in order:
                player moving left
            -}
            case (inputs.left, inputs.right, compare square.vel.x 0) of
                -- no movement desired, already still
                (Inactive, Inactive, EQ) -> (0, 0)
                (Active, Active, EQ) -> (0, 0)
                -- moving to the right
                (Inactive, Active, _) ->
                    let 
                        bumped = square.vel.x + acceleration * delta
                    in
                    if bumped > maxspeed then
                        (maxspeed, 0)
                    else
                        (bumped, acceleration)
                -- moving to the left
                (Active, Inactive, _) ->
                    let 
                        bumped = square.vel.x - acceleration * delta
                    in
                    if bumped < maxspeed * -1 then
                        (maxspeed * -1, 0)
                    else
                        (bumped, acceleration * -1)
                -- no movement desired, moving right
                (_, _, GT) ->
                    let 
                        bumped = square.vel.x - deceleration * delta
                    in
                    if bumped < 0 then
                        (0, 0)
                    else
                        (bumped, deceleration * -1)
                -- no movement desired, moving left
                (_, _, LT) ->
                    let 
                        bumped = square.vel.x + deceleration * delta
                    in
                    if bumped > 0 then
                        (0, 0)
                    else
                        (bumped, deceleration)
        posx = square.pos.x + square.vel.x * delta + accx * delta * delta * 0.5
        posy = square.pos.y + square.vel.y * delta + gravity * delta * delta * 0.5
        vely = square.vel.y + gravity * delta   
    in
    { square | pos = {x = posx, y = posy }, vel = {x = velx, y = vely} }

jump_speed = 2.0 * -1

jump : Bool -> Square -> Square
jump isJump square =
    if isJump then
        let 
            vel = { x = square.vel.x, y = jump_speed}
        in
        { square | vel = vel }
    else
        square

handleKey : MoveInputs -> KeyType -> State -> MoveInputs
handleKey inputs k s =
    {--
    let 
        _ = Debug.log (if s == Active then "Pressed: " else "Release: ") (keyName k)
    in
    --}
    case k of
        Up -> { inputs | up = s }
        Down -> { inputs | down = s }
        Left -> { inputs | left = s }
        Right -> { inputs | right = s }
        Other -> inputs


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
    { title = "Lonely tom"
    , body = 
        [ Html.h1 [] [ Html.text "thom lonely is" ]
        , Html.p [] [ Html.text "Use WASD or arrow key" ]
        , Svg.svg
            [ Attr.width width
            , Attr.height height
            ]
            [ shadow model
            , Svg.rect [ Attr.width squareX, Attr.height squareY, Attr.x x, Attr.y y] []
            ]
        ]
    }

-- shadow : Model -> Svg.msg
shadow model =
    let
        x1 = model.square.pos.x
        y1 = model.square.pos.y + model.square.size.y
        x2 = model.square.pos.x + model.square.size.x
        y2 = model.square.pos.y
        width = model.width
        height = model.height

        (intersectX1, intersectY1) = (rayIntersect x1 y1 width height) 
        (intersectX2, intersectY2) = (rayIntersect x2 y2 width height) 
    in
    Svg.g [] 
    [ Svg.polygon [
        Attr.points (
                    String.join " " 
                    [ point (x1, y1)
                    , point (rayIntersect x1 y1 width height)
                    , point (width, height)
                    , point (rayIntersect x2 y2 width height)
                    , point (x2, y2)
                    ]
                )
        ]
        [] {--
    , Svg.line [ Attr.x1 (String.fromFloat x1), Attr.y1 (String.fromFloat y1), Attr.x2 (String.fromFloat intersectX1), Attr.y2 (String.fromFloat intersectY1)] []
    , Svg.line [ Attr.x1 (String.fromFloat x2), Attr.y1 (String.fromFloat y2), Attr.x2 (String.fromFloat intersectX2), Attr.y2 (String.fromFloat intersectY2)] []
    --}
    ]

point : (Float, Float) -> String
point (x, y) =
    (String.fromFloat x) ++ "," ++ (String.fromFloat y)

rayIntersect : Float -> Float -> Float -> Float -> (Float, Float)
rayIntersect x y width height =
    let
        yIntersect = (y / x) * width
        xIntersect = (x / y) * height
        mag1 = xIntersect ^ 2 + height ^ 2
        mag2 = yIntersect ^ 2 + width  ^ 2
    in
    if mag1 < mag2 then
        (xIntersect, height)
    else
        (width, yIntersect)


-- SUBS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Events.onResize (\w -> \h -> Resize (w, h))
    , Events.onKeyDown (Decode.map keyDown keyDecoder)
    , Events.onKeyUp (Decode.map keyUp keyDecoder)
    , Events.onAnimationFrameDelta (\tick -> Tick tick)
    ]

-- Directions
type KeyType
    = Left
    | Right
    | Up
    | Down
    | Other

keyDown k = KeyEvent (k, Active)
keyUp k = KeyEvent (k, Inactive)

keyName: KeyType -> String
keyName k =
    case k of
    Left  -> "Left"
    Right -> "Right"
    Up    -> "Up"
    Down  -> "Down"
    Other -> "Other"

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
        " " ->
            Up
        _ ->
            Other
