module Render exposing (main)

{-| A quick test of the `Board` rendering code.
-}

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Cards exposing (Card(..), Face(..), Suit(..))
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , col
        , div
        , figcaption
        , figure
        , h2
        , h3
        , img
        , input
        , label
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        , video
        )
import Html.Attributes
    exposing
        ( alt
        , autocomplete
        , autofocus
        , autoplay
        , checked
        , class
        , cols
        , colspan
        , controls
        , disabled
        , draggable
        , height
        , hidden
        , href
        , id
        , name
        , placeholder
        , readonly
        , rows
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (onCheck, onClick, onInput, onMouseDown)
import Random exposing (Seed)
import SayUncle.Board as Board
import SayUncle.Types as Types
    exposing
        ( Board
        , BoardClick(..)
        , GameState
        , Message(..)
        , Player
        , PlayerNames
        , Size
        )
import Task
import Time exposing (Posix)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        , Time.every 1 ReceiveTime
        ]


type alias Model =
    { message : Maybe String
    , windowSize : ( Int, Int )
    , board : Board
    , seed : Seed
    , time : Int
    }


type Msg
    = WindowResize Int Int
    | SuffleTheDeck
    | ReceiveClick BoardClick
    | ReceiveTime Posix


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( board, seed ) =
            Board.empty 2
    in
    { message = Nothing
    , windowSize = ( 1024, 768 )
    , board = board
    , seed = seed
    , time = 0
    }
        |> withCmds
            [ Task.perform ReceiveTime Time.now
            , Task.perform getViewport Dom.getViewport
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize w h ->
            { model | windowSize = ( w, h ) }
                |> withNoCmd

        SuffleTheDeck ->
            let
                ( board, seed ) =
                    Board.initial 2 model.seed
            in
            { model
                | board = board
                , seed = seed
            }
                |> withNoCmd

        ReceiveClick click ->
            -- TODO
            model |> withNoCmd

        ReceiveTime posix ->
            { model
                | time = Time.posixToMillis posix
            }
                |> withNoCmd


view : Model -> Html Msg
view model =
    text ""
