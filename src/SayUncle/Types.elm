---------------------------------------------------------------------
--
-- Types.elm
-- Say Uncle shared types.
-- Copyright (c) 2019-2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module SayUncle.Types exposing
    ( AskYesNo(..)
    , Board
    , BoardClick(..)
    , ChatSettings
    , Choice(..)
    , ConnectionReason(..)
    , ConnectionSpec
    , Game
    , GameInterface
    , GameState
    , InitialBoard
    , Message(..)
    , MessageForLog(..)
    , MessageQueueEntry
    , Model
    , Msg(..)
    , Page(..)
    , Participant
    , Player
    , PlayerNames
    , PrivateGameState
    , PublicGame
    , PublicGameAndPlayers
    , PublicType(..)
    , RowCol
    , SavedModel
    , Score
    , ServerState
    , Settings
    , Size
    , Socket
    , State(..)
    , StatisticsKeys
    , Style
    , StyleType(..)
    , SubscriptionSet
    , WinReason(..)
    , Winner(..)
    , darkStyle
    , emptyPlayerNames
    , emptyPrivateGameState
    , emptySettings
    , gameStateIsVerbose
    , lightStyle
    , messageToGameid
    , messageToPlayer
    , messageToPlayerid
    , posixZero
    , serverIsVerbose
    , statisticsKeyOrder
    , statisticsKeys
    , typeToStyle
    , updateResponseGameState
    , zeroScore
    )

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Cards exposing (Card)
import Deck exposing (ShuffledDeck)
import Dict exposing (Dict)
import ElmChat
import Fifo exposing (Fifo)
import Json.Decode as JD exposing (Decoder, Value)
import PortFunnel.Notification as Notification exposing (Permission(..))
import PortFunnels
import Random exposing (Seed)
import Set exposing (Set)
import Time exposing (Posix, Zone)
import Url exposing (Url)
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , ServerUrl
        , Statistics
        )


type BoardClick
    = TableauClick Card
    | StockClick
    | TurnedStockClick Card
    | HandClick Card


type alias Board =
    { tableau : Array (Maybe Card)
    , stock : ShuffledDeck
    , turnedStock : Maybe Card
    , hands : Array (List Card)
    , seed : Seed
    }


type alias Player =
    Int


type WinReason
    = WinBySayUncle
    | WinByStockUsed


type Winner
    = NoWinner
    | SayUncleWinner { saidUncle : Player, won : Player }
    | StockUsedWinner Player


type Page
    = MainPage
    | RulesPage
    | InstructionsPage
    | PublicPage
    | MovesPage
    | StatisticsPage


type alias Participant =
    Player


type alias Score =
    { games : Int
    , points : Dict Player Int
    }


zeroScore : Score
zeroScore =
    { games = 0
    , points = Dict.empty
    }


type alias Style =
    { backgroundColor : String
    , lineColor : String
    , pathColor : String
    }


lightStyle : Style
lightStyle =
    { backgroundColor = "white"
    , lineColor = "black"
    , pathColor = "orange"
    }


darkStyle : Style
darkStyle =
    { backgroundColor = "black"
    , lineColor = "#BBBBBB"
    , pathColor = "purple"
    }


type StyleType
    = LightStyle
    | DarkStyle
    | CustomStyle Style


typeToStyle : StyleType -> Style
typeToStyle styleType =
    case styleType of
        LightStyle ->
            lightStyle

        DarkStyle ->
            darkStyle

        CustomStyle style ->
            style


type alias Size =
    { width : Int
    , height : Int
    }


type alias Settings =
    { name : String
    , maxPlayers : Int
    , winningPoints : Int
    , isPublic : Bool
    , forName : String
    , hideTitle : Bool
    }


emptySettings : Settings
emptySettings =
    { name = ""
    , maxPlayers = 3
    , winningPoints = 10
    , isPublic = False
    , forName = ""
    , hideTitle = False
    }


type alias SavedModel =
    { page : Page
    , gameid : String
    , settings : Settings
    , styleType : StyleType
    , notificationsEnabled : Bool
    , soundEnabled : Bool
    }



---
--- Talking to the server
---


type alias Socket =
    String


type alias SubscriptionSet =
    Set ( Socket, String )


type alias PrivateGameState =
    { verbose : Maybe Bool
    , subscribers : SubscriptionSet
    , statisticsSubscribers : Set Socket
    , statisticsChanged : Bool

    -- Milliseconds
    , startTime : Maybe Int
    , updateTime : Maybe Int
    }


emptyPrivateGameState : PrivateGameState
emptyPrivateGameState =
    { verbose = Nothing
    , subscribers = Set.empty
    , statisticsSubscribers = Set.empty
    , statisticsChanged = False
    , startTime = Nothing
    , updateTime = Nothing
    }


posixZero : Posix
posixZero =
    Time.millisToPosix 0


type alias InitialBoard =
    { board : Board
    , whoseTurn : Player
    }


type State
    = InitialState
    | TableauState
    | TurnStockState
    | ChooseStockState
    | DiscardState
    | GameOverState Player


type alias GameState =
    { board : Board
    , maxPlayers : Int
    , winningPoints : Int
    , players : PlayerNames
    , whoseTurn : Player
    , player : Player
    , state : State
    , score : Score
    , winner : Winner
    , matchWinner : Maybe Player
    , private : PrivateGameState --not sent over the wire
    }


type alias PlayerNames =
    Dict Int String


emptyPlayerNames : PlayerNames
emptyPlayerNames =
    Dict.empty


type alias RowCol =
    { row : Int
    , col : Int
    }


type PublicType
    = NotPublic
    | EntirelyPublic
    | PublicFor String


updateResponseGameState : (GameState -> GameState) -> Message -> Message
updateResponseGameState updater message =
    case message of
        NewRsp ({ gameState } as rec) ->
            NewRsp { rec | gameState = updater gameState }

        JoinRsp ({ gameState } as rec) ->
            JoinRsp { rec | gameState = updater gameState }

        UpdateRsp ({ gameState } as rec) ->
            UpdateRsp { rec | gameState = updater gameState }

        PlayRsp ({ gameState } as rec) ->
            PlayRsp { rec | gameState = updater gameState }

        AnotherGameRsp ({ gameState } as rec) ->
            AnotherGameRsp { rec | gameState = updater gameState }

        GameOverRsp ({ gameState } as rec) ->
            GameOverRsp { rec | gameState = updater gameState }

        _ ->
            message


type Choice
    = ChooseNew --new game
    | ChooseStart --start game before maxPlayers have joined
    | ChooseTableau Card --choose a tableau card
    | TurnStock --turn the top stock card
    | ChooseStock --choose the turned stock card
    | SkipStock --skip the turned stock card
    | Discard Card --discard after ChooseStock
    | SayUncle --say "Uncle"


type Message
    = NewReq
        { name : String
        , publicType : PublicType
        , maxPlayers : Int
        , winningPoints : Int
        , seedInt : Int
        , restoreState : Maybe GameState
        , maybeGameid : Maybe GameId
        }
    | NewRsp
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        , publicType : PublicType
        , gameState : GameState
        , wasRestored : Bool
        }
    | JoinReq
        { gameid : GameId
        , name : String
        }
    | ReJoinReq
        { gameid : GameId
        , playerid : PlayerId
        }
    | JoinRsp
        { gameid : GameId
        , playerid : PlayerId
        , gameState : GameState
        }
      -- Disallowed if SayUncle.WhichServer.allowGameState is False
    | SetGameStateReq
        { playerid : PlayerId
        , gameState : GameState
        }
    | UpdateReq { playerid : PlayerId }
    | UpdateRsp
        { gameid : String
        , gameState : GameState
        }
      -- Game Play
    | PlayReq
        { playerid : PlayerId
        , placement : Choice
        }
    | PlayRsp
        { gameid : GameId
        , gameState : GameState
        }
    | LeaveReq { playerid : PlayerId }
    | LeaveRsp
        { gameid : GameId
        , participant : Participant
        , name : String
        }
    | AnotherGameRsp
        { gameid : GameId
        , gameState : GameState
        }
    | GameOverRsp
        { gameid : GameId
        , gameState : GameState
        }
      -- Public games
    | PublicGamesReq
        { subscribe : Bool
        , forName : String
        , gameid : Maybe GameId
        }
    | PublicGamesRsp
        { games : List PublicGameAndPlayers
        }
    | PublicGamesUpdateRsp
        { added : List PublicGameAndPlayers
        , removed : List String
        }
    | StatisticsReq
        { subscribe : Bool
        }
    | StatisticsRsp
        { statistics : Maybe Statistics
        , startTime : Maybe Int
        , updateTime : Maybe Int
        }
      -- Errors
    | ErrorRsp
        { request : String
        , text : String
        }
      -- Chat
    | ChatReq
        { playerid : String
        , text : String
        }
    | ChatRsp
        { gameid : String
        , name : String
        , text : String
        }


type alias PublicGame =
    { gameid : GameId
    , creator : String
    , player : Player
    , forName : Maybe String
    }


type alias PublicGameAndPlayers =
    { publicGame : PublicGame
    , players : PlayerNames
    }


messageToPlayer : Message -> Maybe Player
messageToPlayer message =
    case message of
        NewRsp { player } ->
            Just player

        _ ->
            Nothing


messageToPlayerid : Message -> Maybe PlayerId
messageToPlayerid message =
    case message of
        NewRsp { playerid } ->
            Just playerid

        JoinRsp { playerid } ->
            Just playerid

        UpdateReq { playerid } ->
            Just playerid

        PlayReq { playerid } ->
            Just playerid

        ChatReq { playerid } ->
            Just playerid

        _ ->
            Nothing


messageToGameid : Message -> Maybe GameId
messageToGameid message =
    case message of
        NewRsp { gameid } ->
            Just gameid

        ReJoinReq { gameid } ->
            Just gameid

        JoinReq { gameid } ->
            Just gameid

        JoinRsp { gameid } ->
            Just gameid

        UpdateRsp { gameid } ->
            Just gameid

        PlayRsp { gameid } ->
            Just gameid

        GameOverRsp { gameid } ->
            Just gameid

        ChatRsp { gameid } ->
            Just gameid

        _ ->
            Nothing


type MessageForLog
    = NewReqLog
        { name : String
        , publicType : PublicType
        , maxPlayers : Int
        , winningPoints : Int
        , restoreState : Maybe String
        , maybeGameid : Maybe GameId
        }
    | NewRspLog
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        , publicType : PublicType
        , gameState : String
        , wasRestored : Bool
        }
    | JoinReqLog
        { gameid : GameId
        , name : String
        }
    | RejoinReqLog
        { gameid : GameId
        , playerid : PlayerId
        }
    | JoinRspLog
        { gameid : GameId
        , playerid : PlayerId
        , gameState : String
        }
    | SetGameStateReqLog
        { playerid : PlayerId
        , gameState : String
        }
    | UpdateReqLog { playerid : PlayerId }
    | UpdateRspLog
        { gameid : String
        , gameState : String
        }
      -- Game Play
    | PlayReqLog
        { playerid : PlayerId
        , placement : Choice
        }
    | PlayRspLog
        { gameid : GameId
        , gameState : String
        }
    | LeaveReqLog
        { playerid : PlayerId
        }
    | LeaveRspLog
        { gameid : GameId
        , participant : Participant
        , name : String
        }
    | AnotherGameRspLog
        { gameState : String
        }
    | GameOverRspLog
        { gameid : GameId
        , gameState : String
        }
      -- Public games
    | PublicGamesReqLog
        { subscribe : Bool
        , forName : String
        , gameid : Maybe GameId
        }
    | PublicGamesRspLog { games : List PublicGameAndPlayers }
    | PublicGamesUpdateRspLog
        { added : List PublicGameAndPlayers
        , removed : List String
        }
    | StatisticsReqLog
        { subscribe : Bool
        }
    | StatisticsRspLog
        { statistics : Maybe Statistics
        , startTime : Maybe Int
        , updateTime : Maybe Int
        }
      -- Errors
    | ErrorRspLog
        { request : String
        , text : String
        }
      -- Chat
    | ChatReqLog
        { playerid : String
        , text : String
        }
    | ChatRspLog
        { gameid : String
        , name : String
        , text : String
        }


type alias ServerState =
    WebSocketFramework.Types.ServerState GameState Player


serverIsVerbose : ServerState -> Bool
serverIsVerbose state =
    case state.state of
        Nothing ->
            False

        Just gs ->
            gameStateIsVerbose gs


gameStateIsVerbose : GameState -> Bool
gameStateIsVerbose gs =
    case gs.private.verbose of
        Nothing ->
            False

        Just v ->
            v


type alias StatisticsKeys =
    { finishedGames : String
    , totalMoves : String
    , activeGames : String
    , totalConnections : String
    , totalPublicConnections : String
    , activeConnections : String
    }


statisticsKeys : StatisticsKeys
statisticsKeys =
    { finishedGames = "Finished Games"
    , totalMoves = "Finished Game Total Moves"
    , activeGames = "Active Games"
    , totalConnections = "Total Sessions"
    , totalPublicConnections = "Total Public Sessions"
    , activeConnections = "Active Sessions"
    }


statisticsKeyOrder : List (StatisticsKeys -> String)
statisticsKeyOrder =
    [ .finishedGames
    , .totalMoves
    , .activeGames
    , .totalConnections
    , .totalPublicConnections
    , .activeConnections
    ]


type alias GameInterface =
    WebSocketFramework.Types.ServerInterface GameState Player Message Msg


type alias ChatSettings =
    ElmChat.Settings Msg


type alias Game =
    { gameid : GameId
    , playerIds : Dict PlayerId Player --used in local mode
    , gameState : GameState
    , isLocal : Bool
    , serverUrl : String
    , player : Player
    , playerid : PlayerId
    , isLive : Bool

    -- Not persistent
    , interfaceIsProxy : Bool
    , interface : GameInterface
    }



{--I usually put these in Main.elm, but UI.elm is split out, so they're shared now.
--}


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


type AskYesNo a
    = AskAsk
    | AskYes a
    | AskNo


type alias MessageQueueEntry =
    { isLocal : Bool
    , isSend : Bool
    , message : Message
    }


type alias Model =
    { tick : Posix
    , zone : Zone
    , delayTime : Posix
    , game : Game
    , gameDict : Dict String Game
    , chatSettings : ChatSettings
    , connectionSpecQueue : Fifo ConnectionSpec
    , funnelState : PortFunnels.State
    , key : Key
    , windowSize : ( Int, Int )
    , started : Bool --True when persistent storage is available
    , error : Maybe String
    , publicGames : List PublicGameAndPlayers
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


type Msg
    = Noop
    | Tick Posix
    | IncomingMessage Bool GameInterface Message
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
    | MakeInitialBoard
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
