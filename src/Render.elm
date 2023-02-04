module Render exposing (main)

{-| A quick test of the `Board` rendering code.
-}

import Array exposing (Array)
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Cards exposing (Card(..), Face(..), Suit(..))
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Deck exposing (ShuffledDeck)
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
import Html.Lazy as Lazy
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


type State
    = TableauState
    | TurnStockState
    | ChooseStockState
    | DiscardState
    | ScoreState


type alias Model =
    { message : Maybe String
    , windowSize : Size
    , gameState : GameState
    , seed : Seed
    , time : Int
    , whoseTurn : Player
    , playerNames : PlayerNames
    , state : State
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
    , whoseTurn = 0
    , playerNames = Dict.fromList [ ( 0, "Bill" ), ( 1, "Tom" ) ]
    , state = TableauState
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


nextPlayer : Model -> Model
nextPlayer model =
    let
        whoseTurn =
            model.whoseTurn

        maxPlayer =
            Dict.size model.playerNames - 1
    in
    { model
        | whoseTurn =
            if whoseTurn >= maxPlayer then
                0

            else
                whoseTurn + 1
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize w h ->
            { model | windowSize = Debug.log "WindowResize" <| Size w h }
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
            let
                gameState =
                    model.gameState

                board =
                    gameState.board

                whoseTurn =
                    model.whoseTurn
            in
            case click of
                TableauClick card ->
                    case Array.get whoseTurn board.hands of
                        Nothing ->
                            model |> withNoCmd

                        Just cards ->
                            let
                                tableau =
                                    Array.map
                                        (\maybeCard ->
                                            if Just card == maybeCard then
                                                Nothing

                                            else
                                                maybeCard
                                        )
                                        board.tableau

                                newBoard =
                                    { board
                                        | tableau = tableau
                                        , hands =
                                            Array.set whoseTurn
                                                (Board.sortCards <| card :: cards)
                                                board.hands
                                    }
                            in
                            { model
                                | gameState =
                                    { gameState | board = newBoard }
                                , state =
                                    if Board.isTableauEmpty tableau then
                                        TurnStockState

                                    else
                                        model.state
                            }
                                |> nextPlayer
                                |> withNoCmd

                StockClick ->
                    case model.state of
                        ChooseStockState ->
                            nextPlayer model
                                |> withNoCmd

                        TurnStockState ->
                            let
                                ( card, newStock ) =
                                    Deck.draw board.stock
                            in
                            if card == Back then
                                -- can't happen
                                model |> withNoCmd

                            else
                                { model
                                    | state = ChooseStockState
                                    , gameState =
                                        { gameState
                                            | board =
                                                { board
                                                    | stock = newStock
                                                    , turnedStock = Just card
                                                }
                                        }
                                }
                                    |> withNoCmd

                        _ ->
                            model |> withNoCmd

                TurnedStockClick card ->
                    if model.state /= ChooseStockState then
                        model |> withNoCmd

                    else
                        -- TODO
                        model |> withNoCmd

                HandClick card ->
                    model |> withNoCmd

        ReceiveTime posix ->
            let
                time =
                    Time.posixToMillis posix

                ( gameState, seed2 ) =
                    if model.time == 0 then
                        let
                            seed =
                                Random.initialSeed time

                            gs =
                                model.gameState

                            ( board, seed3 ) =
                                Board.initial 2 seed
                        in
                        ( { gs | board = board }
                        , seed3
                        )

                    else
                        ( model.gameState, model.seed )
            in
            { model
                | time = time
                , gameState = gameState
                , seed = seed2
            }
                |> withNoCmd


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    div []
        [ Lazy.lazy3 (Board.render ReceiveClick)
            model.windowSize
            model.whoseTurn
            model.gameState
        , div
            [ style "width" "100%"
            , style "margin" "auto"
            , style "text-align" "center"
            , style "font-weight" "bold"
            ]
            [ text <| Board.getPlayerName model.whoseTurn model.playerNames ]
        ]
