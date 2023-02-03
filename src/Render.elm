module Render exposing (main)

{-| A quick test of the `Board` rendering code.
-}

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Cards exposing (Card(..), Face(..), Suit(..))
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
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
        , Score
        , Size
        , Winner(..)
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
    , windowSize : Size
    , gameState : GameState
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
    , windowSize = Size 1024 768
    , gameState =
        { board = Board.empty 2 |> Tuple.first
        , players = Dict.fromList [ ( 0, "Bill" ), ( 2, "Tom" ) ]
        , whoseTurn = 0
        , score = Types.zeroScore
        , winner = NoWinner
        , private = Types.emptyPrivateGameState
        }
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
            { model | windowSize = Size w h }
                |> withNoCmd

        SuffleTheDeck ->
            let
                ( board, seed ) =
                    Board.initial 2 model.seed

                gameState =
                    model.gameState
            in
            { model
                | gameState =
                    { gameState | board = board }
                , seed = seed
            }
                |> withNoCmd

        ReceiveClick click ->
            -- TODO
            model |> withNoCmd

        ReceiveTime posix ->
            let
                time =
                    Time.posixToMillis posix

                gameState =
                    model.gameState

                ( board, seed2 ) =
                    if model.time == 0 then
                        let
                            seed =
                                Random.initialSeed time
                        in
                        Board.initial 2 seed

                    else
                        ( gameState.board, model.seed )
            in
            { model
                | time = time
                , gameState =
                    { gameState | board = board }
                , seed = seed2
            }
                |> withNoCmd


view : Model -> Html Msg
view model =
    Board.render ReceiveClick model.windowSize model.gameState
