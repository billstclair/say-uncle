---------------------------------------------------------------------
--
-- Main.elm
-- Say Uncle top-level
-- Copyright (c) 2019-2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


port module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import DateFormat
import DateFormat.Relative
import Dict exposing (Dict)
import Dict.Extra as DE
import ElmChat exposing (LineSpec(..), defaultExtraAttributes)
import Fifo exposing (Fifo)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , audio
        , blockquote
        , button
        , div
        , embed
        , fieldset
        , h1
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , optgroup
        , option
        , p
        , select
        , source
        , span
        , table
        , td
        , text
        , textarea
        , tr
        )
import Html.Attributes as Attributes
    exposing
        ( align
        , alt
        , autofocus
        , autoplay
        , checked
        , class
        , cols
        , colspan
        , disabled
        , height
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
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Html.Lazy as Lazy
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import List.Extra as LE
import Markdown
import PortFunnel.LocalStorage as LocalStorage exposing (Label)
import PortFunnel.Notification as Notification exposing (Permission(..))
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Random exposing (Seed)
import SayUncle.Board as Board
import SayUncle.EncodeDecode as ED
import SayUncle.Interface as Interface
import SayUncle.Types as Types
    exposing
        ( Board
        , BoardClick(..)
        , Choice(..)
        , GameState
        , Message(..)
        , NamedGame
        , Page(..)
        , Participant
        , Player
        , PlayerNames
        , PublicGame
        , PublicGameAndPlayers
        , PublicType(..)
        , RowCol
        , SavedModel
        , Score
        , ServerState
        , Settings
        , StatisticsKeys
        , Style
        , StyleType(..)
        , WinReason(..)
        , Winner(..)
        , statisticsKeys
        )
import SayUncle.WhichServer as WhichServer
import Svg exposing (Svg, foreignObject, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , fontSize
        , height
        , stroke
        , strokeDasharray
        , strokeWidth
        , textAnchor
        , transform
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )
import Svg.Button as SB exposing (Button, Content(..))
import Svg.Events
import Task
import Time exposing (Month(..), Posix, Zone)
import Url exposing (Url)
import WebSocketFramework
import WebSocketFramework.EncodeDecode as WSFED
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , GameId
        , MessageDecoder
        , MessageEncoder
        , PlayerId
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        , Statistics
        )


port onVisibilityChange : (Bool -> msg) -> Sub msg


port playAudio : String -> Cmd msg


type alias ServerInterface =
    Types.ServerInterface Msg


type ConnectionReason
    = StartGameConnection
    | JoinGameConnection GameId Bool
    | PublicGamesConnection
    | StatisticsConnection
    | UpdateConnection PlayerId
    | RestoreGameConnection Game
    | JoinRestoredGameConnection GameId


type alias ConnectionSpec =
    { connectionReason : ConnectionReason
    }


type alias ChatSettings =
    Types.ChatSettings Msg


type AskYesNo a
    = AskAsk
    | AskYes a
    | AskNo


type alias Game =
    NamedGame Msg


type alias MessageQueueEntry =
    { isLocal : Bool
    , isSend : Bool
    , message : Message
    }


type alias Model =
    { tick : Posix
    , zone : Zone
    , game : Game
    , gameDict : Dict String Game
    , chatSettings : ChatSettings
    , connectionSpecQueue : Fifo ConnectionSpec
    , funnelState : State
    , key : Key
    , windowSize : ( Int, Int )
    , started : Bool --True when persistent storage is available
    , error : Maybe String
    , publicGames : List PublicGameAndPlayers
    , time : Posix
    , requestedNew : Bool
    , reallyClearStorage : Bool
    , statistics : Maybe Statistics
    , statisticsTimes : ( Maybe Int, Maybe Int )
    , notificationAvailable : Maybe Bool
    , notificationPermission : Maybe Permission
    , visible : Bool
    , soundFile : Maybe String
    , messageQueue : Fifo MessageQueueEntry
    , showMessageQueue : Bool

    -- persistent below here
    , page : Page
    , gameid : String
    , settings : Settings
    , styleType : StyleType
    , notificationsEnabled : Bool
    , soundEnabled : Bool
    }


isPlaying : Model -> Bool
isPlaying model =
    let
        game =
            model.game

        gameState =
            game.gameState
    in
    game.isLive && Dict.size gameState.players == gameState.maxPlayers


type Msg
    = Noop
    | Tick Posix
    | IncomingMessage Bool ServerInterface Message
    | RecordMessage Bool Bool Message
    | SetIsLocal Bool
    | SetDarkMode Bool
    | SetName String
    | SetIsPublic Bool
    | SetForName String
    | SetServerUrl String
    | SetGameid String
    | SetPage Page
    | SetHideTitle Bool
    | NewGame
    | StartGame
    | Join
    | JoinGame GameId
    | Disconnect
    | SetShowMessageQueue Bool
    | SetNotificationsEnabled Bool
    | SetSoundEnabled Bool
    | InitialBoard
    | Reload
    | MaybeClearStorage
    | ClearStorage
    | Click BoardClick
    | ChatUpdate ChatSettings (Cmd Msg)
    | ChatSend String ChatSettings
    | ChatClear
    | PlaySound String
    | DelayedAction (Model -> ( Model, Cmd Msg )) Posix
    | SetZone Zone
    | WindowResize Int Int
    | VisibilityChange Bool
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | DoConnectedResponse
    | RestoreSubscriptions
    | Process Value


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


view : Model -> Document Msg
view model =
    { title = "Say Uncle"
    , body =
        div []
            [ text "Say Uncle User Interface under construction."
            ]
    }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = ED.messageEncoderWithPrivate
    , decoder = ED.messageDecoder
    , errorWrapper = Just errorMessageEncoder
    }


errorMessageEncoder : WebSocketFramework.Types.Error Message -> Message
errorMessageEncoder error =
    ErrorRsp
        { request = "Unknown"
        , text =
            Debug.toString error.kind
                ++ ":"
                ++ error.description
                ++ " / "
                ++ (case error.message of
                        Ok message ->
                            Debug.toString message

                        Err err ->
                            err
                   )
        }


fullProcessor : ServerMessageProcessor GameState Participant Message
fullProcessor =
    ServerInterface.fullMessageProcessor encodeDecode Interface.proxyMessageProcessor


updateServerState : (ServerState -> ServerState) -> ServerInterface -> ServerInterface
updateServerState updater serverInterface =
    let
        (ServerInterface interface) =
            serverInterface
    in
    case interface.state of
        Nothing ->
            let
                state =
                    WebSocketFramework.Types.emptyServerState Nothing
            in
            ServerInterface
                { interface
                    | state = Just <| updater state
                }

        Just state ->
            ServerInterface
                { interface
                    | state = Just <| updater state
                }


updateServerTime : Posix -> ServerInterface -> ServerInterface
updateServerTime posix serverInterface =
    serverInterface
        |> updateServerState (\state -> { state | time = posix })


updateServerSeed : Maybe Seed -> ServerInterface -> ServerInterface
updateServerSeed maybeSeed serverInterface =
    case maybeSeed of
        Nothing ->
            serverInterface

        Just seed ->
            serverInterface
                |> updateServerState (\state -> { state | seed = seed })


proxyServer : Maybe Seed -> ServerInterface
proxyServer seed =
    ServerInterface.makeProxyServer fullProcessor (IncomingMessage True)
        |> updateServerSeed seed


updateChatAttributes : Int -> StyleType -> ChatSettings -> ChatSettings
updateChatAttributes bsize styleType settings =
    let
        renderStyle =
            Types.typeToStyle styleType

        attributes =
            settings.attributes
    in
    { settings
        | attributes =
            { attributes
                | chatTable =
                    [ style "width" "fit-content"
                    , style "max-width" "90%"
                    , style "margin" "auto"
                    ]
                , textColumn =
                    [ style "width" "fit-content"
                    ]
                , textArea =
                    [ style "width" <| String.fromInt (5 * bsize // 6) ++ "px"
                    , style "height" "6em"
                    , style "border-color" renderStyle.lineColor
                    ]
            }
    }



-- TODO
-- Use UI.ids


ids =
    { chatOutput = "chatOutput"
    , chatInput = "chatInput"
    , forName = "forName"
    }


initialChatSettings : Zone -> ChatSettings
initialChatSettings zone =
    let
        settings =
            ElmChat.makeSettings ids.chatOutput 14 True ChatUpdate
    in
    { settings | zone = zone }


initialGame : Maybe Seed -> Game
initialGame seed =
    { gameid = ""
    , playerIds = Dict.empty
    , playerWins = Dict.empty
    , gameState = Interface.emptyGameState Types.emptyPlayerNames
    , isLocal = False
    , serverUrl = WhichServer.serverUrl
    , player = 0
    , playerid = ""
    , isLive = False
    , yourWins = 0

    -- not persistent
    , interfaceIsProxy = True
    , interface = proxyServer seed
    }


insertConnectionSpec : ConnectionSpec -> Model -> Model
insertConnectionSpec spec model =
    { model
        | connectionSpecQueue =
            Fifo.insert spec model.connectionSpecQueue
    }


removeConnectionSpec : Model -> ( Maybe ConnectionSpec, Model )
removeConnectionSpec model =
    let
        ( spec, queue ) =
            Fifo.remove model.connectionSpecQueue
    in
    ( spec, { model | connectionSpecQueue = queue } )


isConnectionSpecQueueEmpty : Model -> Bool
isConnectionSpecQueueEmpty model =
    model.connectionSpecQueue == Fifo.empty


zeroTick : Posix
zeroTick =
    Time.millisToPosix 0


makeSeed : Posix -> Seed
makeSeed posix =
    Random.initialSeed (Time.posixToMillis posix)


modelSeed : Model -> Maybe Seed
modelSeed model =
    if model.tick == zeroTick then
        Nothing

    else
        Just <| makeSeed model.tick


init : Value -> url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        game =
            initialGame Nothing

        model =
            { tick = zeroTick
            , zone = Time.utc
            , game = game
            , gameDict = Dict.empty
            , chatSettings =
                initialChatSettings Time.utc
            , connectionSpecQueue = Fifo.empty
            , funnelState = initialFunnelState
            , key = key
            , windowSize = ( 0, 0 )
            , started = False
            , error = Nothing
            , publicGames = []
            , time = Time.millisToPosix 0
            , requestedNew = False
            , reallyClearStorage = False
            , statistics = Nothing
            , statisticsTimes = ( Nothing, Nothing )
            , notificationAvailable = Nothing
            , notificationPermission = Nothing
            , visible = True
            , soundFile = Nothing
            , messageQueue = Fifo.empty
            , showMessageQueue = False

            -- persistent fields
            , page = MainPage
            , gameid = ""
            , settings = Types.emptySettings
            , styleType = LightStyle
            , notificationsEnabled = False
            , soundEnabled = False
            }
    in
    model
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            , Task.perform SetZone Time.here
            , Task.perform Tick Time.now
            ]


type alias NewReqBody =
    { name : String
    , publicType : PublicType
    , maxPlayers : Int
    , winningPoints : Int
    , seed : Seed
    , restoreState : Maybe GameState
    , maybeGameid : Maybe GameId
    }


initialNewReqBody : Int -> Int -> NewReqBody
initialNewReqBody maxPlayers winningPoints =
    { name = ""
    , publicType = NotPublic
    , maxPlayers = maxPlayers
    , winningPoints = winningPoints
    , restoreState = Nothing
    , maybeGameid = Nothing
    }


initialNewReq : Message
initialNewReq =
    NewReq initialNewReqBody


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if LocalStorage.isLoaded state.storage then
                        True

                    else
                        model.started
            }

        cmd =
            if mdl.started && not model.started && model.tick /= zeroTick then
                get pk.model

            else
                Cmd.none
    in
    case response of
        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys mdl

        LocalStorage.GetResponse { label, key, value } ->
            case value of
                Nothing ->
                    mdl |> withNoCmd

                Just v ->
                    handleGetResponse label key v model

        _ ->
            mdl |> withCmd cmd


handleListKeysResponse : Label -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse label prefix keys model =
    case label of
        Nothing ->
            model |> withNoCmd

        Just lab ->
            if lab == pk.game then
                let
                    getter key cmd =
                        Cmd.batch
                            [ cmd
                            , getLabeled pk.game key
                            ]

                    getCmds =
                        List.foldr getter Cmd.none <| Debug.log "Getting games" keys
                in
                ( model, getCmds )

            else
                model |> withNoCmd


handleGetResponse : Label -> String -> Value -> Model -> ( Model, Cmd Msg )
handleGetResponse label key value model =
    case label of
        Just lab ->
            if lab == pk.game then
                handleGetGameResponse key value model

            else if lab == pk.chat then
                handleGetChatResponse key value model

            else
                model |> withNoCmd

        Nothing ->
            if key == pk.model then
                let
                    cmd =
                        listKeysLabeled pk.game gamePrefix
                in
                case ED.decodeSavedModel value of
                    Err e ->
                        model |> withCmd cmd

                    Ok savedModel ->
                        savedModelToModel savedModel model
                            |> withCmd cmd

            else
                model |> withNoCmd


updateGame : (Game -> Game) -> Model -> ( Model, Maybe Game )
updateGame updater model =
    let
        game =
            updater model.game
    in
    ( { model | game = game }
    , Just game
    )


handleGetGameResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetGameResponse _ value model =
    let
        foo =
            Debug.log "handleGetGameResponse"
    in
    case
        JD.decodeValue (ED.namedGameDecoder <| proxyServer (modelSeed model))
            value
    of
        Err _ ->
            { model
                | error = Just <| "Couldn't decode game."
            }
                |> withNoCmd

        Ok game ->
            let
                model2 =
                    { model
                        | game = game
                    }

                model3 =
                    -- Empty chat in case it's not in localStorage
                    { model2
                        | chatSettings =
                            initialChatSettings model2.zone
                    }

                getChatCmd =
                    getChat

                ( model4, cmd4 ) =
                    reconnectToGame game model3
            in
            model4 |> withCmds [ getChatCmd, cmd4 ]


maybeRestoreSubscriptions : Model -> ( Model, Cmd Msg )
maybeRestoreSubscriptions model =
    let
        restoreSubscription connectionType =
            model
                |> webSocketConnect
                    model.game
                    (ConnectionSpec connectionType)
    in
    if model.page == PublicPage then
        restoreSubscription PublicGamesConnection

    else if model.page == StatisticsPage then
        restoreSubscription StatisticsConnection

    else
        model |> withNoCmd


reconnectToGame : Game -> Model -> ( Model, Cmd Msg )
reconnectToGame game model =
    if not game.isLocal && game.isLive && game.playerid /= "" then
        model
            |> webSocketConnect
                game
                (ConnectionSpec <| UpdateConnection game.playerid)

    else if game.isLocal then
        model
            |> withCmd (initialNewReqCmd game model)

    else
        -- Need to set gameid to ""?
        maybeRestoreSubscriptions model


updateChat : Model -> (ChatSettings -> ChatSettings) -> ( Model, ChatSettings )
updateChat model updater =
    let
        chat =
            updater model.chatSettings
    in
    ( { model
        | chatSettings = chat
      }
    , chat
    )


updateChat2 : Model -> (ChatSettings -> ( ChatSettings, a )) -> ( Model, Maybe ( ChatSettings, a ) )
updateChat2 model updater =
    let
        ( chat, a ) =
            updater model.chatSettings
    in
    ( { model
        | chatSettings = chat
      }
    , Just ( chat, a )
    )


handleGetChatResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetChatResponse _ value model =
    case JD.decodeValue (ElmChat.settingsDecoder ChatUpdate) value of
        Err _ ->
            { model
                | error = Just <| "Couldn't decode chat."
            }
                |> withNoCmd

        Ok settings ->
            let
                ( model2, chat2 ) =
                    updateChat
                        model
                        (\chat ->
                            { settings
                                | id = ids.chatOutput
                                , zone = chat.zone
                            }
                        )
            in
            model2
                |> withCmd (ElmChat.restoreScroll chat2)


initialNewReqCmd : Game -> Model -> Cmd Msg
initialNewReqCmd game model =
    send game.isLocal game.interface <|
        let
            req =
                initialNewReqBody
        in
        NewReq
            { req
                | restoreState =
                    Just game.gameState
            }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { page = model.page
    , chooseFirst = model.chooseFirst
    , gameid = model.gameid
    , settings = model.settings
    , styleType = model.styleType
    , notificationsEnabled = model.notificationsEnabled
    , soundEnabled = model.soundEnabled
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    let
        game =
            model.game
    in
    { model
        | page = savedModel.page
        , chooseFirst = savedModel.chooseFirst
        , lastTestMode = savedModel.lastTestMode
        , gameid = savedModel.gameid
        , settings = savedModel.settings
        , styleType = savedModel.styleType
        , notificationsEnabled = savedModel.notificationsEnabled
        , soundEnabled = savedModel.soundEnabled
    }


playerName : Player -> Game -> Maybe String
playerName player game =
    Dict.get player game.gameState.players


localizedPlayerName : Player -> Game -> String
localizedPlayerName player game =
    case playerName player game of
        Nothing ->
            ""

        Just name ->
            if name == "" || game.isLocal || player /= game.player then
                name

            else
                "You (" ++ name ++ ")"


incomingMessage : ServerInterface -> Message -> Model -> ( Model, Cmd Msg )
incomingMessage interface message mdl =
    let
        messageLog =
            Debug.log "incomingMessage" <| ED.messageToLogMessage message

        model =
            { mdl | reallyClearStorage = False }

        ( maybeGame, ( model2, cmd2 ) ) =
            case Types.messageToGameid message of
                Nothing ->
                    incomingMessageInternal interface Nothing message model

                Just gameid ->
                    let
                        maybeGame2 =
                            if gameid == model.gameid then
                                Nothing

                            else
                                let
                                    game =
                                        model.game
                                in
                                Just { game | interface = interface }
                    in
                    incomingMessageInternal interface maybeGame2 message model
    in
    case maybeGame of
        Nothing ->
            model2 |> withCmd cmd2

        Just game ->
            let
                ( model3, _ ) =
                    updateGame game model2
            in
            model3
                |> withCmds [ cmd2, putGame game ]


{-| Do the work for `incomingMessage`.

If `maybeGame` is not not `Nothing`, then a game with its `gameid` was found.
If the `Maybe Game` in the result is not `Nothing`, then the `Game` was changed,
and needs to be persisted by `incomingMessage`. Otherwise, it was NOT changed.

-}
incomingMessageInternal : ServerInterface -> Maybe Game -> Message -> Model -> ( Maybe Game, ( Model, Cmd Msg ) )
incomingMessageInternal interface maybeGame message model =
    let
        withRequiredGame gameid thunk =
            case maybeGame of
                Just game ->
                    thunk game

                Nothing ->
                    ( Nothing
                    , { model
                        | error =
                            Just <| "Bug: there is no session for id: " ++ gameid
                      }
                        |> withNoCmd
                    )
    in
    case message of
        NewRsp { gameid, playerid, player, name, gameState, wasRestored } ->
            if maybeGame /= Nothing && not wasRestored then
                case maybeGame of
                    Nothing ->
                        -- Can't happen
                        ( Nothing, model |> withNoCmd )

                    Just game ->
                        let
                            returnedGame =
                                if game.isLocal then
                                    -- Otherwise, there's no way out in the UI.
                                    Just { game | isLive = False }

                                else
                                    Nothing
                        in
                        ( returnedGame
                        , { model
                            | error =
                                -- This is actually remotely possible, if the
                                -- remote server happens to randomly generate a
                                -- gameid that matches a local one (or vice-versa).
                                Just <| "Bug: NewRsp found existing session id: " ++ gameid
                          }
                            |> withNoCmd
                        )

            else
                let
                    game =
                        model.game

                    ( model2, chatCmd ) =
                        clearChatSettings True model

                    game2 =
                        { game
                            | gameid = gameid
                            , gameState = gameState
                            , player = player
                            , playerid = playerid
                            , isLive = True
                            , yourWins = 0
                            , interface = interface
                        }

                    model3 =
                        { model2 | gameid = gameid }
                in
                ( Just game2
                , model3
                    |> withCmds
                        [ chatCmd
                        , if not game.isLocal then
                            Cmd.none

                          else if player == WhitePlayer then
                            send game2.isLocal interface <|
                                JoinReq
                                    { gameid = gameid
                                    , name = "Black"
                                    }

                          else
                            send game2.isLocal interface <|
                                JoinReq
                                    { gameid = gameid
                                    , name = "White"
                                    }
                        ]
                )

        JoinRsp { gameid, playerid, gameState } ->
            let
                game =
                    model.game

                ( model2, chatCmd ) =
                    if not wasRestored then
                        clearChatSettings True model

                    else
                        model |> withNoCmd

                newPlayer =
                    if playerid == Nothing then
                        game.player

                    else
                        player

                game2 =
                    { game
                        | gameid = gameid
                        , gameState = gameState
                        , isLive = True
                        , player = newPlayer
                        , yourWins = 0
                        , interface = interface
                    }

                game3 =
                    if game2.isLocal then
                        { game2
                            | otherPlayerid =
                                case playerid of
                                    Just p ->
                                        p

                                    Nothing ->
                                        ""
                        }

                    else
                        case playerid of
                            Nothing ->
                                game2

                            Just pid ->
                                { game2 | playerid = pid }

                ( model3, _ ) =
                    updateGame game3 model2

                msg =
                    "The game is on!"
            in
            ( Just game3
            , { model3 | error = error }
                |> withCmds
                    [ chatCmd
                    , maybeSendNotification game3 False msg model2
                    ]
            )

        LeaveRsp { gameid, participant } ->
            let
                body : Game -> Player -> ( Maybe Game, ( Model, Cmd Msg ) )
                body game player =
                    let
                        name =
                            playerName
                                (Types.otherPlayer game.player)
                                game

                        leftMsg =
                            name ++ " left" ++ "."

                        game2 =
                            { game
                                | gameid = ""
                                , playerid = ""
                            }

                        model2 =
                            { model
                                | error =
                                    if player == game2.player then
                                        Nothing

                                    else
                                        Just leftMsg
                            }
                    in
                    if game2.isLocal then
                        ( Just game2, model2 |> withNoCmd )

                    else
                        ( Just { game2 | isLive = False }
                        , model2
                            |> withCmds
                                [ if player /= game2.player then
                                    maybeSendNotification game2 True leftMsg model2

                                  else
                                    Cmd.none
                                ]
                        )

                err : Game -> String -> ( Maybe Game, ( Model, Cmd Msg ) )
                err game name =
                    let
                        isYourGame =
                            Just name == game.watcherName

                        ( game2, needsUpdate ) =
                            if isYourGame then
                                ( { game
                                    | gameid = ""
                                    , playerid = ""
                                    , isLive = False
                                    , watcherName = Nothing
                                  }
                                , True
                                )

                            else
                                ( game, False )
                    in
                    ( if needsUpdate then
                        Just game2

                      else
                        Nothing
                    , { model
                        | error =
                            if isYourGame then
                                Just "You stopped spectating."

                            else
                                Just <| "Spectator left: " ++ name ++ "."
                      }
                        |> withNoCmd
                    )
            in
            withRequiredGame gameid
                (\game ->
                    Interface.ensuringPlayer participant (err game) (body game)
                )

        UpdateRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    ( Just { game | gameState = gameState }
                    , model
                        |> withCmd
                            (Task.perform identity <|
                                Task.succeed RestoreSubscriptions
                            )
                    )
                )

        PlayRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    let
                        sound =
                            if gameState.whoseTurn /= game.gameState.whoseTurn then
                                Task.perform PlaySound <| Task.succeed "sounds/move.mp3"

                            else if
                                (gameState.jumps /= [])
                                    && (gameState.jumps /= game.gameState.jumps)
                            then
                                Task.perform PlaySound <| Task.succeed "sounds/jump.mp3"

                            else
                                Cmd.none

                        error =
                            if game.watcherName == Nothing then
                                Nothing

                            else
                                case gameState.requestUndo of
                                    NoRequestUndo ->
                                        Nothing

                                    RequestUndo msg ->
                                        Just <| "Undo requested: " ++ maybeNoText msg

                                    DenyUndo msg ->
                                        Just <| "Undo denied: " ++ maybeNoText msg
                    in
                    if not game.isLocal then
                        ( Just
                            { game
                                | gameState = gameState
                                , yourWins = computeYourWins gameState model
                            }
                        , { model | error = error }
                            |> withCmds
                                [ maybeSendNotification
                                    game
                                    False
                                    "It's your turn in AGOG."
                                    model
                                , sound
                                ]
                        )

                    else
                        ( Just { game | gameState = gameState }
                        , model
                            |> withCmd sound
                        )
                )

        ResignRsp { gameid, gameState, player } ->
            withRequiredGame gameid
                (\game ->
                    let
                        resignMsg =
                            if game.player == player then
                                "You resigned."

                            else
                                playerName player game ++ " resigned."
                    in
                    ( Just
                        { game
                            | gameState = gameState
                            , yourWins =
                                computeYourWins gameState model
                        }
                    , { model
                        | error =
                            if game.isLocal then
                                Nothing

                            else
                                Just resignMsg
                      }
                        |> withCmd (maybeSendNotification game True resignMsg model)
                    )
                )

        AnotherGameRsp { gameid, gameState, player } ->
            withRequiredGame gameid
                (\game ->
                    let
                        ( error, msg ) =
                            if not game.isLocal && not model.requestedNew then
                                let
                                    m =
                                        playerName (Types.otherPlayer player)
                                            { game | gameState = gameState }
                                            ++ " asked for a new game"
                                in
                                ( Just m, m )

                            else
                                ( Nothing, "" )

                        mdl2 =
                            { model
                                | requestedNew = False
                                , error = error
                            }

                        cmd =
                            if error == Nothing then
                                Cmd.none

                            else
                                maybeSendNotification game True msg mdl2
                    in
                    ( Just
                        { game
                            | gameState = gameState
                            , player = player
                            , archives =
                                if game.gameState.moves == [] then
                                    game.archives

                                else
                                    Interface.archiveGame game.gameState
                                        :: game.archives
                        }
                    , mdl2 |> withCmd cmd
                    )
                )

        GameOverRsp { gameid, gameState } ->
            withRequiredGame gameid
                (\game ->
                    ( Just { game | gameState = gameState }
                    , model |> withNoCmd
                    )
                )

        PublicGamesRsp { games } ->
            ( Nothing
            , { model | publicGames = games }
                |> withNoCmd
            )

        PublicGamesUpdateRsp { added, removed } ->
            let
                games =
                    List.filter
                        (\pgaps ->
                            not <| List.member pgaps.publicGame.gameid removed
                        )
                        model.publicGames
            in
            ( Nothing
            , { model | publicGames = List.concat [ games, added ] }
                |> withNoCmd
            )

        StatisticsRsp { statistics, startTime, updateTime } ->
            ( Nothing
            , { model
                | statistics = statistics
                , statisticsTimes = ( startTime, updateTime )
              }
                |> withNoCmd
            )

        ErrorRsp { request, text } ->
            let
                errorReturn () =
                    ( Nothing
                    , { model | error = Just text }
                        |> withNoCmd
                    )
            in
            case WSFED.decodeMessage ED.messageDecoder request of
                Ok (UpdateReq { playerid }) ->
                    -- Server has forgotten the game.
                    -- Restore it.
                    if model.playerId == "" then
                        ( Nothing
                        , { model
                            | error =
                                Just "Bug: Can't restore session."
                          }
                            |> withNoCmd
                        )

                    else
                        ( Nothing
                        , webSocketConnect
                            model.game
                            (ConnectionSpec <|
                                RestoreGameConnection game
                            )
                            model
                        )

                Ok (NewReq { maybeGameid }) ->
                    case maybeGameid of
                        Nothing ->
                            errorReturn ()

                        Just gameid ->
                            if gameid /= model.gameid then
                                errorReturn ()

                            else
                                let
                                    restoredGame =
                                        model.game
                                in
                                case playerName restoredGame.player restoredGame of
                                    "" ->
                                        errorReturn ()

                                    _ ->
                                        ( Nothing
                                        , webSocketConnect
                                            restoredGame
                                            (ConnectionSpec <|
                                                JoinRestoredGameConnection gameid
                                            )
                                            model
                                        )

                Ok (LeaveReq { playerid }) ->
                    let
                        game =
                            model.game
                    in
                    ( Just
                        { game
                            | gameid = ""
                            , playerid = ""
                            , isLive = False
                            , watcherName = Nothing
                        }
                    , model |> withNoCmd
                    )

                _ ->
                    errorReturn ()

        ChatRsp { gameid, name, text } ->
            withRequiredGame gameid
                (\game ->
                    let
                        ( model2, ( _, cmd ) ) =
                            updateChat2
                                model
                                (\chat ->
                                    ElmChat.addLineSpec chat <|
                                        ElmChat.makeLineSpec text
                                            (Just name)
                                            (Just model.time)
                                )
                    in
                    ( Nothing
                    , case maybeTuple of
                        Nothing ->
                            model2 |> withNoCmd
                    , model2
                        |> withCmds
                            [ cmd

                            -- Kluge. ElmChat is supposed to do this
                            , Task.attempt (\_ -> Noop) <|
                                Dom.setViewportOf ids.chatOutput 0 1000000
                            , maybeSendNotification game
                                True
                                ("You got a SayUncle chat message from " ++ name)
                                model
                            ]
                    )
                )

        _ ->
            ( Nothing, model |> withNoCmd )


computeYourWins : GameState -> Model -> Int
computeYourWins gameState model =
    let
        game =
            model.game

        winner =
            gameState.winner
    in
    if
        (winner /= NoWinner)
            && (game.gameState.winner == NoWinner)
            && Just game.player
            == Types.winPlayer winner
    then
        game.yourWins + 1

    else
        game.yourWins


setPage : Page -> Cmd Msg
setPage page =
    Task.perform SetPage <| Task.succeed page


notificationHandler : Notification.Response -> State -> Model -> ( Model, Cmd Msg )
notificationHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        Notification.NoResponse ->
            model |> withNoCmd

        Notification.AvailableResponse available ->
            { model | notificationAvailable = Just available }
                |> withNoCmd

        Notification.PermissionResponse permission ->
            let
                enabled =
                    case model.notificationPermission of
                        Nothing ->
                            permission == PermissionGranted

                        _ ->
                            model.notificationsEnabled
            in
            { model
                | notificationPermission =
                    Just permission
                , error =
                    if permission == PermissionDenied then
                        Just "You denied notification permission. This can only be changed in your brower's settings."

                    else
                        model.error
                , notificationsEnabled = enabled
            }
                |> withCmd
                    (if enabled && not model.notificationsEnabled then
                        sendNotification "Notifications Enabled!"

                     else
                        Cmd.none
                    )

        Notification.NotificationResponse notification ->
            let
                n =
                    Debug.log "notification" notification
            in
            model |> withNoCmd

        Notification.ClickResponse id ->
            model
                |> withCmds
                    [ setPage MainPage
                    , notificationCmd (Notification.dismissNotification id)
                    ]

        Notification.ErrorResponse s ->
            { model | error = Just <| "Notification error: " ++ s }
                |> withNoCmd


{-| TODO: properly notify crowd members on unshown session change.
-}
maybeSendNotification : Game -> Bool -> String -> Model -> Cmd Msg
maybeSendNotification game ignoreWhoseTurn title model =
    if
        model.notificationsEnabled
            && (game.watcherName == Nothing)
            && (ignoreWhoseTurn || game.gameState.whoseTurn == game.player)
            && not game.isLocal
            && not model.visible
    then
        sendNotification title

    else
        Cmd.none


sendNotification : String -> Cmd Msg
sendNotification title =
    Notification.displayNotification title
        |> notificationCmd


notificationCmd : Notification.Message -> Cmd Msg
notificationCmd message =
    message
        |> Notification.send (getCmdPort Notification.moduleName ())


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        ErrorResponse error ->
            case error of
                WebSocket.SocketAlreadyOpenError _ ->
                    socketHandler
                        (ConnectedResponse { key = "", description = "" })
                        state
                        model

                _ ->
                    { model | error = Just <| WebSocket.errorToString error }
                        |> withNoCmd

        WebSocket.MessageReceivedResponse received ->
            let
                string =
                    received.message
            in
            case WSFED.decodeMessage ED.messageDecoder string of
                Err errmsg ->
                    { model | error = Just errmsg }
                        |> withNoCmd

                Ok message ->
                    if game.isLocal then
                        { model
                            | error =
                                Just "Got a WebSocket message for a local game."
                        }
                            |> withNoCmd

                    else
                        let
                            game =
                                model.game
                        in
                        { model | error = Nothing }
                            |> withCmd
                                (Task.perform (IncomingMessage False game.interface) <|
                                    Task.succeed message
                                )

        ClosedResponse { expected, reason } ->
            { model
                | connectionSpecQueue = Fifo.empty
                , error =
                    if Debug.log "ClosedResponse, expected" expected then
                        model.error

                    else
                        Just <| "Connection unexpectedly closed: " ++ reason
            }
                |> updateGame
                    (\g ->
                        if g.isLocal then
                            g

                        else
                            { g | isLive = False }
                    )
                |> withNoCmd

        ConnectedResponse crrec ->
            connectedResponse model

        _ ->
            model |> withNoCmd


connectedResponse : Model -> ( Model, Cmd Msg )
connectedResponse model =
    let
        ( maybeConnectionSpec, model2 ) =
            removeConnectionSpec model
    in
    { model2 | error = Nothing }
        |> withCmds
            [ if isConnectionSpecQueueEmpty model2 then
                Cmd.none

              else
                Task.perform identity <|
                    Task.succeed DoConnectedResponse
            , case maybeConnectionSpec of
                Nothing ->
                    Cmd.none

                Just { connectionReason } ->
                    processConnectionReason model.game connectionReason model2
            ]


processConnectionReason : Game -> ConnectionReason -> Model -> Cmd Msg
processConnectionReason game connectionReason model =
    let
        interface =
            game.interface

        isLocal =
            game.isLocal
    in
    case Debug.log "processConnectionReason" connectionReason of
        StartGameConnection ->
            let
                settings =
                    model.settings
            in
            send isLocal interface <|
                NewReq
                    { name = model.settings.name
                    , player = model.chooseFirst
                    , publicType =
                        if not settings.isPublic then
                            NotPublic

                        else
                            case settings.forName of
                                "" ->
                                    EntirelyPublic

                                forName ->
                                    PublicFor forName
                    , restoreState = Nothing
                    , maybeGameid = Nothing
                    }

        JoinGameConnection gameid inCrowd ->
            send isLocal interface <|
                JoinReq
                    { gameid = gameid
                    , name = model.settings.name
                    , isRestore = False
                    , inCrowd = inCrowd
                    }

        PublicGamesConnection ->
            send isLocal interface <|
                PublicGamesReq
                    { subscribe = model.page == PublicPage
                    , forName = model.settings.name
                    , gameid = Just model.game.gameid
                    }

        StatisticsConnection ->
            send isLocal interface <|
                StatisticsReq
                    { subscribe = model.page == StatisticsPage
                    }

        UpdateConnection playerid ->
            send isLocal interface <|
                UpdateReq
                    { playerid = playerid }

        RestoreGameConnection localGame ->
            case game.watcherName of
                Just _ ->
                    processConnectionReason game
                        (JoinRestoredGameConnection game.gameid)
                        model

                Nothing ->
                    let
                        player =
                            localGame.player

                        name =
                            playerName player localGame
                    in
                    if name == "" then
                        Cmd.none

                    else
                        send isLocal interface <|
                            NewReq
                                { name = name
                                , player = player
                                , publicType = NotPublic
                                , restoreState = Just localGame.gameState
                                , maybeGameid = Just localGame.gameid
                                }

        JoinRestoredGameConnection gameid ->
            -- Errors are generated in ErrorRsp handler,
            -- before it generates the Cmd that gets here.
            if gameid /= model.game.gameid then
                Cmd.none

            else
                let
                    localGame =
                        model.game
                in
                case playerName localGame.player localGame of
                    Nothing ->
                        Cmd.none

                    Just name ->
                        send isLocal interface <|
                            JoinReq
                                { gameid = gameid
                                , name = name
                                , isRestore = True
                                }


focusId : String -> Cmd Msg
focusId id =
    Task.attempt (\_ -> Noop) (Dom.focus id)


onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
    on "keydown" (JD.map tagger keyCode)


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger keycode =
            if keycode == 13 then
                msg

            else
                Noop
    in
    onKeydown tagger


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        clearClearStorage =
            model.reallyClearStorage
                && (case msg of
                        ClearStorage ->
                            False

                        Tick _ ->
                            False

                        _ ->
                            True
                   )

        ( mdl, cmd ) =
            updateInternal msg <|
                if clearClearStorage then
                    { model | reallyClearStorage = False, error = Nothing }

                else
                    model

        { white, black } =
            mdl.game.gameState.players

        focus =
            --not game.isLocal && game.isLive && white /= "" && black /= ""
            --Might be able to be smart and do this just on desktop, but not for now.
            --Focusing on mobile zooms the screen and shows the keyboard.
            False

        doSave =
            case msg of
                Noop ->
                    False

                Tick _ ->
                    model.tick == zeroTick

                Click _ ->
                    cmd == Cmd.none

                NewGame ->
                    --False
                    True

                Process _ ->
                    False

                IncomingMessage _ _ _ ->
                    -- cmd == Cmd.none
                    True

                Reload ->
                    False

                MaybeClearStorage ->
                    False

                ClearStorage ->
                    False

                ChatUpdate _ _ _ ->
                    False

                ChatSend _ _ ->
                    False

                ChatClear ->
                    False

                PlaySound _ ->
                    False

                DelayedAction _ _ ->
                    False

                _ ->
                    True
    in
    mdl
        |> withCmds
            [ cmd
            , if focus && doSave then
                focusId ids.chatInput

              else
                Cmd.none
            , if model.started && doSave then
                putModel mdl

              else
                Cmd.none
            ]


messageQueueLength : Int
messageQueueLength =
    20


fifoLength : Fifo a -> Int
fifoLength fifo =
    List.length <| Fifo.toList fifo


recordMessage : Bool -> Bool -> Message -> Model -> Model
recordMessage interfaceIsLocal isSend message model =
    let
        queue =
            if fifoLength model.messageQueue >= messageQueueLength then
                let
                    ( _, q ) =
                        Fifo.remove model.messageQueue
                in
                q

            else
                model.messageQueue
    in
    { model
        | messageQueue =
            Fifo.insert (MessageQueueEntry interfaceIsLocal isSend message) queue
    }


showingArchiveOrMove : Model -> Bool
showingArchiveOrMove model =
    model.showArchive /= Nothing || model.showMove /= Nothing


isWatcher : Model -> Bool
isWatcher model =
    model.game.watcherName /= Nothing


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    let
        game =
            model.game

        gameState =
            game.gameState

        settings =
            model.settings
    in
    case msg of
        Noop ->
            model |> withNoCmd

        Tick posix ->
            { model
                | tick = posix
                , game =
                    if model.tick /= zeroTick then
                        if not game.isLocal then
                            game

                        else
                            { game
                                | interface =
                                    updateServerTime model.tick game.interface
                            }

                    else
                        { game
                            | interface =
                                updateServerSeed (Just <| makeSeed posix)
                                    game.interface
                        }
            }
                |> withCmd
                    (if model.tick == zeroTick && model.started then
                        -- This is also done by storageHandler,
                        -- but only if the seed has been
                        -- initialized.
                        get pk.model

                     else
                        Cmd.none
                    )

        IncomingMessage interfaceIsLocal interface message ->
            incomingMessage interface message <|
                recordMessage interfaceIsLocal False message model

        RecordMessage interfaceIsLocal isSend message ->
            recordMessage interfaceIsLocal isSend message model
                |> withNoCmd

        SetIsLocal isLocal ->
            if game.isLocal == isLocal || showingArchiveOrMove model then
                model |> withNoCmd

            else
                let
                    interface =
                        if isLocal then
                            proxyServer <| modelSeed model

                        else
                            game.interface

                    game2 =
                        { game
                            | isLocal = isLocal
                            , isLive = False
                            , playerid = ""
                            , watcherName = Nothing
                            , interface = interface
                            , interfaceIsProxy = isLocal
                        }

                    model2 =
                        { model
                            | game = game2
                            , gameid = ""
                        }
                in
                model2
                    |> withCmds
                        [ putGame game2
                        , if isLocal && not game.isLocal then
                            Cmd.batch
                                [ if game.isLive then
                                    send game.isLocal interface <|
                                        LeaveReq { playerid = game.playerid }

                                  else
                                    Cmd.none
                                , initialNewReqCmd game2 model
                                ]

                          else
                            Cmd.none
                        ]

        SetDarkMode darkMode ->
            let
                styleType =
                    if darkMode then
                        DarkStyle

                    else
                        LightStyle
            in
            { model | styleType = styleType }
                |> withNoCmd

        SetName name ->
            { model | settings = { settings | name = name } }
                |> withNoCmd

        SetIsPublic isPublic ->
            { model | settings = { settings | isPublic = isPublic } }
                |> withCmd
                    (if isPublic && not settings.isPublic then
                        focusId ids.forName

                     else
                        Cmd.none
                    )

        SetForName forName ->
            { model | settings = { settings | forName = forName } }
                |> withNoCmd

        SetServerUrl serverUrl ->
            { model | game = { game | serverUrl = serverUrl } }
                |> withNoCmd

        SetGameid gameid ->
            { model | gameid = gameid }
                |> withNoCmd

        SetPage page ->
            let
                mdl =
                    { model | page = page }

                ( mdl2, cmd ) =
                    if page == PublicPage then
                        webSocketConnect
                            model.game
                            (ConnectionSpec PublicGamesConnection)
                            mdl

                    else if page == StatisticsPage then
                        webSocketConnect
                            model.game
                            (ConnectionSpec StatisticsConnection)
                            mdl

                    else
                        ( mdl, Cmd.none )

                interface =
                    mdl2.game.interface

                cmd2 =
                    if page == StatisticsPage then
                        send game.isLocal interface <|
                            StatisticsReq { subscribe = True }

                    else if model.page == StatisticsPage then
                        send game.isLocal interface <|
                            StatisticsReq { subscribe = False }

                    else if page == MainPage then
                        let
                            chat =
                                model.chatSettings
                        in
                        ElmChat.restoreScroll chat

                    else
                        Cmd.none

                cmd3 =
                    if page == PublicPage then
                        send game.isLocal interface <|
                            PublicGamesReq
                                { subscribe = True
                                , forName = ""
                                , gameid = Nothing
                                }

                    else if model.page == PublicPage then
                        send game.isLocal interface <|
                            PublicGamesReq
                                { subscribe = False
                                , forName = ""
                                , gameid = Nothing
                                }

                    else
                        Cmd.none
            in
            mdl2 |> withCmds [ cmd, cmd2, cmd3 ]

        SetHideTitle hideTitle ->
            { model | settings = { settings | hideTitle = hideTitle } }
                |> withNoCmd

        NewGame ->
            let
                resigning =
                    if not game.isLocal then
                        game.player

                    else
                        gameState.whoseTurn

                pid =
                    if not game.isLocal then
                        game.playerid

                    else
                        case resigning of
                            WhitePlayer ->
                                game.playerid

                            BlackPlayer ->
                                game.otherPlayerid

                ( playerid, placement ) =
                    if gameState.winner == NoWinner then
                        ( pid
                        , ChooseResign resigning
                        )

                    else
                        let
                            player =
                                if game.isLocal then
                                    WhitePlayer

                                else
                                    -- This should probably be enforced
                                    -- by the server.
                                    Types.otherPlayer game.player
                        in
                        ( game.playerid, ChooseNew player )
            in
            { model | requestedNew = True }
                |> withCmd
                    (send model.game.isLocal model.game.interface <|
                        PlayReq
                            { playerid = playerid
                            , placement = placement
                            }
                    )

        StartGame ->
            startGame model

        Join ->
            join model False

        JoinGame gameid ->
            join { model | gameid = gameid } False

        Disconnect ->
            if showingArchiveOrMove model then
                model |> withNoCmd

            else
                disconnect model

        SetNotificationsEnabled enabled ->
            if not enabled then
                { model | notificationsEnabled = False }
                    |> withNoCmd

            else
                case model.notificationPermission of
                    Nothing ->
                        model
                            |> withCmd
                                (notificationCmd Notification.requestPermission)

                    Just PermissionDenied ->
                        { model | notificationsEnabled = False }
                            |> withNoCmd

                    _ ->
                        { model | notificationsEnabled = enabled }
                            |> withCmd
                                (if enabled then
                                    sendNotification "Notifications Enabled!"

                                 else
                                    Cmd.none
                                )

        SetShowMessageQueue showMessageQueue ->
            { model | showMessageQueue = showMessageQueue }
                |> withNoCmd

        SetSoundEnabled bool ->
            { model | soundEnabled = bool }
                |> withNoCmd

        InitialBoard ->
            let
                game2 =
                    { game
                        | gameState =
                            { gameState
                                | moves = []
                                , newBoard = Board.initial
                                , selected = Nothing
                                , whoseTurn = WhitePlayer
                                , legalMoves = NoMoves
                            }
                    }
            in
            { model | game = game2 }
                |> withCmd (putGame game2)

        Reload ->
            model |> withCmd Navigation.reloadAndSkipCache

        MaybeClearStorage ->
            { model
                | reallyClearStorage = True
                , error = Just "Click the \"Clear Storage Now!\" button to clear all storage."
            }
                |> withNoCmd

        ClearStorage ->
            let
                ( mdl, cmd ) =
                    init JE.null "url" model.key
            in
            { mdl
                | started = True
                , windowSize = model.windowSize
                , notificationAvailable = model.notificationAvailable
                , tick = model.tick
            }
                |> withCmds [ clearStorage ]

        Click boardClick ->
            if gameState.winner /= NoWinner || (not <| isPlaying model) then
                model |> withNoCmd

            else
                doClick boardClick model

        ChatUpdate chatSettings cmd ->
            updateChat model (always chatSettings)
                |> Tuple.first
                |> withCmds [ cmd, putChat chatSettings ]

        ChatSend line chatSettings ->
            chatSend line chatSettings model

        ChatClear ->
            clearChatSettings False model

        PlaySound file ->
            if not model.soundEnabled then
                model |> withNoCmd

            else
                { model | soundFile = Just file }
                    |> withCmd (playAudio file)

        DelayedAction updater time ->
            updater { model | time = time }

        SetZone zone ->
            let
                chat =
                    model.chatSettings
            in
            { model
                | zone = zone
                , chatSettings = { chat | zone = zone }
            }
                |> withNoCmd

        WindowResize w h ->
            { model | windowSize = ( w, h ) }
                |> withNoCmd

        VisibilityChange visibility ->
            { model | visible = visibility }
                |> withNoCmd

        HandleUrlRequest request ->
            ( model
            , case request of
                Internal url ->
                    -- For now
                    Navigation.load <| Url.toString url

                External urlString ->
                    Navigation.load urlString
            )

        HandleUrlChange url ->
            model |> withNoCmd

        DoConnectedResponse ->
            connectedResponse model

        RestoreSubscriptions ->
            maybeRestoreSubscriptions model

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    -- Maybe we should display an error here,
                    -- but I don't think it will ever happen.
                    model |> withNoCmd

                Ok res ->
                    res


clearChatSettings : Bool -> Model -> ( Model, Cmd Msg )
clearChatSettings clearInput model =
    let
        ( model2, chat ) =
            updateChat
                model
                (\chat2 ->
                    { chat2
                        | lines = []
                        , input =
                            if clearInput then
                                ""

                            else
                                chat2.input
                    }
                )
    in
    model2 |> withCmd (putChat chat)


chatSend : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSend line chatSettings model =
    model
        |> withCmd (delayedAction <| chatSendInternal line chatSettings)


chatSendInternal : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSendInternal line chatSettings model =
    let
        ( model2, _ ) =
            updateChat model (always chatSettings)
    in
    model2
        |> withCmd
            (send model2.game.isLocal model2.game.interface <|
                ChatReq
                    { playerid = model.game.playerid
                    , text = line
                    }
            )


delayedAction : (Model -> ( Model, Cmd Msg )) -> Cmd Msg
delayedAction updater =
    Task.perform (DelayedAction updater) Time.now


makeWebSocketServer : Model -> ServerInterface
makeWebSocketServer model =
    WebSocketFramework.makeServer
        (getCmdPort WebSocket.moduleName ())
        ED.messageEncoder
        model.game.serverUrl
        Noop


webSocketConnect : Game -> ConnectionSpec -> Model -> ( Model, Cmd Msg )
webSocketConnect game spec model =
    if game.isLocal then
        let
            newGame =
                { game
                    | interface =
                        if game.interfaceIsProxy then
                            game.interface

                        else
                            proxyServer <| modelSeed model
                    , interfaceIsProxy = True
                    , isLive = True
                }
        in
        updateGame game model
            |> Tuple.first
            |> withNoCmd

    else
        let
            newGame =
                { game
                    | interface =
                        if True then
                            --game.interfaceIsProxy then
                            makeWebSocketServer model

                        else
                            game.interface
                    , interfaceIsProxy = False
                }
        in
        updateGame newGame model
            |> Tuple.first
            |> insertConnectionSpec (Debug.log "webSocketConnect" spec)
            |> withCmd
                (WebSocket.makeOpen game.serverUrl
                    |> webSocketSend
                )


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    webSocketConnect model.game
        (ConnectionSpec StartGameConnection)
        model


join : Model -> Bool -> ( Model, Cmd Msg )
join model inCrowd =
    let
        gameid =
            model.gameid

        game =
            model.game

        model2 =
            { model
                | game =
                    -- Needed by JoinRsp
                    { game | gameid = gameid }
            }

        ( model3, cmd3 ) =
            webSocketConnect model2.game
                (ConnectionSpec <|
                    JoinGameConnection gameid inCrowd
                )
                model2
    in
    model3 |> withCmds [ cmd3, setPage MainPage ]


disconnect : Model -> ( Model, Cmd Msg )
disconnect model =
    let
        game =
            model.game

        gameState =
            if not game.isLocal then
                game.gameState

            else
                let
                    gs =
                        game.gameState
                in
                { gs | score = Types.zeroScore }

        game2 =
            if not game.isLocal then
                { game
                    | isLive = False
                }

            else
                { game
                    | archives = []
                    , gameState = gameState
                }
    in
    { model | game = game2 }
        |> withCmds
            [ putGame game2
            , if game.isLive && not game.isLocal then
                send model.game.isLocal model.game.interface <|
                    LeaveReq { playerid = game.playerid }

              else
                Cmd.none
            , if game.isLocal then
                send model.game.isLocal model.game.interface <|
                    SetGameStateReq
                        { playerid = game.playerid
                        , gameState = gameState
                        }

              else
                Cmd.none
            ]


send : Bool -> ServerInterface -> Message -> Cmd Msg
send interfaceIsLocal interface message =
    let
        logMessage =
            Debug.log "send" <| ED.messageToLogMessage message
    in
    Cmd.batch
        [ ServerInterface.send interface message
        , Task.perform (RecordMessage interfaceIsLocal True) <| Task.succeed message
        ]


doClick : BoardClick -> Model -> ( Model, Cmd Msg )
doClick boardClick model =
    -- TODO
    model |> withNoCmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        , onVisibilityChange VisibilityChange -- Browser.Events.onVisibilityChange doesn't work
        , PortFunnels.subscriptions Process model
        , Time.every 900 Tick
        ]



---
--- Persistence
---


putModel : Model -> Cmd Msg
putModel model =
    let
        savedModel =
            modelToSavedModel model

        value =
            ED.encodeSavedModel savedModel
    in
    put pk.model <| Just value


putChat : ChatSettings -> Cmd Msg
putChat settings =
    (Just <| ElmChat.settingsEncoder settings)
        |> put pk.chat


getChat : Cmd Msg
getChat =
    getLabeled pk.chat pk.chat


putGame : Game -> Cmd Msg
putGame game =
    (Just <| ED.encodeNamedGame game)
        |> put pk.game


getGame : Cmd Msg
getGame =
    getLabeled pk.game pk.game


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend <| Debug.log "LocalStorage" (LocalStorage.get key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (Debug.log "LocalStorage" <|
            LocalStorage.getLabeled label key
        )


listKeys : String -> Cmd Msg
listKeys prefix =
    localStorageSend (LocalStorage.listKeys prefix)


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


clearStorage : Cmd Msg
clearStorage =
    localStorageSend (LocalStorage.clear "")


localStoragePrefix : String
localStoragePrefix =
    "SayUncle"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


webSocketSend : WebSocket.Message -> Cmd Msg
webSocketSend message =
    WebSocket.send (getCmdPort WebSocket.moduleName ()) <|
        Debug.log "webSocketSend" message


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort Process moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        , NotificationHandler notificationHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "model"
    , chat = "chat"
    , game = "game"
    }
