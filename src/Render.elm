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
        , State(..)
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


initialMessage : String
initialMessage =
    "Click a Tableau card to add it to your hand."


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( board, seed ) =
            Board.empty 2
    in
    { message = Just initialMessage
    , windowSize = Size 1024 768
    , gameState =
        { board = Board.empty 2 |> Tuple.first
        , players = Dict.fromList [ ( 0, "Bill" ), ( 1, "Tom" ) ]
        , whoseTurn = 0
        , player = 0
        , state = TableauState
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


nextPlayer : Player -> PlayerNames -> Player
nextPlayer player players =
    let
        maxPlayer =
            Dict.size players - 1
    in
    if player >= maxPlayer then
        0

    else
        player + 1


setNextPlayer : Model -> Model
setNextPlayer model =
    let
        gameState =
            model.gameState
    in
    { model
        | gameState =
            { gameState
                | player =
                    nextPlayer gameState.player gameState.players
            }
    }


turnStockMessage : String
turnStockMessage =
    "Click the stock to turn over the top card."


chooseStockMessage : String
chooseStockMessage =
    "Click the card to add it to your hand. Click the pile to pass."


discardMessage : String
discardMessage =
    "Click a card in your hand to discard."


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        gameState =
            model.gameState
    in
    case msg of
        WindowResize w h ->
            { model | windowSize = Debug.log "WindowResize" <| Size w h }
                |> withNoCmd

        SuffleTheDeck ->
            let
                ( board, seed ) =
                    Board.initial 2 model.seed
            in
            { model
                | message = Just initialMessage
                , gameState =
                    { gameState | board = board }
                , seed = seed
            }
                |> withNoCmd

        ReceiveClick click ->
            let
                board =
                    gameState.board

                whoseTurn =
                    gameState.whoseTurn

                player =
                    gameState.player
            in
            case click of
                TableauClick card ->
                    case Array.get player board.hands of
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
                                            Array.set player
                                                (Board.sortCards <| card :: cards)
                                                board.hands
                                    }

                                ( message, state ) =
                                    if Board.isTableauEmpty tableau then
                                        ( Just turnStockMessage
                                        , TurnStockState
                                        )

                                    else
                                        ( model.message, gameState.state )
                            in
                            { model
                                | message = message
                                , gameState =
                                    { gameState
                                        | board = newBoard
                                        , state = state
                                    }
                            }
                                |> setNextPlayer
                                |> withNoCmd

                StockClick ->
                    case gameState.state of
                        ChooseStockState ->
                            let
                                mdl =
                                    setNextPlayer model
                            in
                            if mdl.gameState.player == mdl.gameState.whoseTurn then
                                let
                                    mdl2 =
                                        setNextPlayer mdl

                                    gs =
                                        mdl2.gameState
                                in
                                { mdl2
                                    | message = Just turnStockMessage
                                    , gameState =
                                        { gs
                                            | whoseTurn = gs.player
                                            , state = TurnStockState
                                            , board =
                                                { board
                                                    | turnedStock = Nothing
                                                }
                                        }
                                }
                                    |> withNoCmd

                            else
                                mdl |> withNoCmd

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
                                    | message =
                                        Just chooseStockMessage
                                    , gameState =
                                        { gameState
                                            | state = ChooseStockState
                                            , board =
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
                    if gameState.state /= ChooseStockState then
                        model |> withNoCmd

                    else
                        let
                            hands =
                                board.hands

                            newBoard =
                                case Array.get player hands of
                                    Nothing ->
                                        board

                                    Just cards ->
                                        let
                                            newCards =
                                                card :: cards |> Board.sortCards
                                        in
                                        { board
                                            | hands =
                                                Array.set player newCards hands
                                            , turnedStock = Nothing
                                        }
                        in
                        { model
                            | message =
                                Just discardMessage
                            , gameState =
                                { gameState
                                    | board = newBoard
                                    , state = DiscardState
                                }
                        }
                            |> withNoCmd

                HandClick card ->
                    if gameState.state /= DiscardState then
                        model |> withNoCmd

                    else
                        let
                            hands =
                                board.hands
                        in
                        case Array.get player hands of
                            Nothing ->
                                model |> withNoCmd

                            Just cards ->
                                let
                                    newCards =
                                        List.filter ((/=) card) cards

                                    newWhoseTurn =
                                        nextPlayer whoseTurn
                                            gameState.players
                                in
                                { model
                                    | message =
                                        Just turnStockMessage
                                    , gameState =
                                        { gameState
                                            | board =
                                                { board
                                                    | hands =
                                                        Array.set player newCards hands
                                                }
                                            , whoseTurn = newWhoseTurn
                                            , player = newWhoseTurn
                                            , state = TurnStockState
                                        }
                                }
                                    |> withNoCmd

        ReceiveTime posix ->
            let
                time =
                    Time.posixToMillis posix

                ( gameState2, seed2 ) =
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
                        ( gameState, model.seed )
            in
            { model
                | time = time
                , gameState = gameState2
                , seed = seed2
            }
                |> withNoCmd


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    let
        gameState =
            model.gameState

        board =
            gameState.board

        playerNameHtml player =
            text <| Board.getPlayerName player gameState.players

        playerNameDiv player =
            div
                [ style "width" "100%"
                , style "margin" "auto"
                , style "text-align" "center"
                , style "font-weight" "bold"
                ]
                [ playerNameHtml player ]

        otherPlayer =
            nextPlayer gameState.player gameState.players
    in
    div []
        [ case model.message of
            Nothing ->
                text ""

            Just message ->
                p
                    [ style "color" "red"
                    , style "text-align" "center"
                    ]
                    [ text message ]
        , Lazy.lazy3 (Board.render ReceiveClick)
            model.windowSize
            model.gameState.player
            model.gameState
        , playerNameDiv gameState.player
        , br
        , Board.renderPlayerHand ReceiveClick
            model.windowSize
            otherPlayer
            gameState.players
            board.hands
        , playerNameDiv otherPlayer
        ]
