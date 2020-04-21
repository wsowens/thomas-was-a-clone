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

zeroVector = { x = 0, y = 0 }

mag : Vector -> Float
mag v = sqrt (v.x ^ 2 + v.y ^ 2)

toString : Vector -> String
toString v = (String.fromFloat v.x) ++ "," ++ (String.fromFloat v.y)

type alias Square =
    { pos : Vector
    , vel : Vector
    , size : Vector
    }

type State
    = Active
    | Inactive

type alias MoveInputs =
    { up : State
    , down : State
    , left : State
    , right : State
    }

type alias Model =
    { square : Square
    , width : Float
    , height : Float
    , inputs : MoveInputs
    }

-- INIT

defaultSize = { x = 50, y = 100 }
start = { up = Inactive, down = Inactive, left = Inactive, right = Inactive}

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
        KeyEvent (k, s) ->
            let
                inputs = handleKey (model.inputs) k s
                square = jump ( inputs.up == Active && model.inputs.up == Inactive ) model.square
            in
            ( { model | inputs = inputs, square = square }
            , Cmd.none
            )
        Tick d ->
            ( boundsCheck { model | square = (timeStep model.square model.inputs d) }
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
        (lower, vel * -0.2)
    else if x > upper then
        (upper, vel * -0.2)
    else
        (x, vel)

handleKey : MoveInputs -> KeyType -> State -> MoveInputs
handleKey inputs k s =
    case k of
        Up -> { inputs | up = s }
        Down -> { inputs | down = s }
        Left -> { inputs | left = s }
        Right -> { inputs | right = s }

jump_speed = 2.0 * -1
jump : Bool -> Square -> Square
jump doJump square =
    if doJump then
        let 
            vel = { x = square.vel.x, y = jump_speed}
        in
        { square | vel = vel }
    else
        square

gravity = 0.005
maxspeed = 1.2
acceleration = 0.007
deceleration = 0.004
timeStep : Square -> MoveInputs -> Float -> Square
timeStep square inputs delta =
    let
        (velx, accx) =
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

-- VIEW

view: Model -> Browser.Document Msg
view model =
    { title = "Lonely tom"
    , body = 
        [ Html.h1 [] [ Html.text "Thomas was a Clone" ], Html.p [] [ Html.text "Use WASD or arrow keys" ]
        , Svg.svg [Attr.width (String.fromFloat model.width), Attr.height (String.fromFloat model.height)] 
            [ drawShadow model, drawSquare model.square ]
        ]
    }

drawSquare : Square -> Svg.Svg msg
drawSquare s = 
    Svg.rect [ Attr.width (String.fromFloat s.size.x), Attr.height (String.fromFloat s.size.y),
               Attr.x (String.fromFloat s.pos.x), Attr.y (String.fromFloat s.pos.y) ] []

drawShadow : Model -> Svg.Svg msg
drawShadow model =
    let
        p1 = Vector   model.square.pos.x ( model.square.pos.y + model.square.size.y )
        p2 = Vector ( model.square.pos.x + model.square.size.x ) model.square.pos.y
        width = model.width
        height = model.height
        intersect1 = (rayIntersect p1 width height)
        intersect2 = (rayIntersect p2 width height) 
        points = List.map toString [p1, intersect1, {x = width, y = height}, intersect2, p2]
    in
    Svg.polygon [Attr.points (String.join " " points)] []

rayIntersect : Vector -> Float -> Float -> Vector
rayIntersect v width height =
    let
        intersect1 = {x = width, y = (v.y / v.x) * width}
        intersect2 = {x = (v.x / v.y) * height, y = height}
    in
    -- the lower of the two points means less off-screen rendering
    if (mag intersect1) < (mag intersect2) then
        intersect1
    else
        intersect2

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

keyDown k = KeyEvent (k, Active)
keyUp k = KeyEvent (k, Inactive)

keyDecoder : Decode.Decoder KeyType
keyDecoder =
    Decode.field "key" Decode.string |> Decode.andThen toDirection

toDirection : String -> Decode.Decoder KeyType
toDirection string =
    case String.toLower string of
        "a" ->
            Decode.succeed Left
        "arrowleft" ->
            Decode.succeed Left
        "d" ->
            Decode.succeed Right
        "arrowright" ->
            Decode.succeed Right
        "w" ->
            Decode.succeed Up
        "arrowup" ->
            Decode.succeed Up
        "s" ->
            Decode.succeed Down
        "arrowdown" ->
            Decode.succeed Down
        " " ->
            Decode.succeed Up
        _ ->
            Decode.fail "invalid key"
