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
    ( ArchivedGame
    , Board
    , BoardClick(..)
    , ChatSettings
    , GameState
    , InitialBoard
    , Message(..)
    , MessageForLog(..)
    , NamedGame
    , Page(..)
    , Participant(..)
    , Player
    , PlayerNames
    , PrivateGameState
    , PublicGame
    , PublicGameAndPlayers
    , PublicType(..)
    , RowCol
    , SavedModel
    , Score
    , ServerInterface
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
    , defaultGamename
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
import Cards exposing (Card)
import Deck exposing (ShuffledDeck)
import Dict exposing (Dict)
import ElmChat
import Set exposing (Set)
import Time exposing (Posix)
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
    }


type alias Player =
    Int


type Participant
    = PlayingParticipant Player
    | CrowdParticipant String


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


type alias Score =
    { games : Int
    , points : Dict Int Int
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
    , isPublic : Bool
    , forName : String
    , hideTitle : Bool
    }


emptySettings : Settings
emptySettings =
    { name = ""
    , isPublic = False
    , forName = ""
    , hideTitle = False
    }


type alias SavedModel =
    { gamename : String
    , gameGamename : String
    , page : Page
    , chooseFirst : Player
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
    = TableauState
    | TurnStockState
    | ChooseStockState
    | DiscardState
    | ScoreState


type alias GameState =
    { board : Board
    , players : PlayerNames
    , whoseTurn : Player
    , player : Player
    , state : State
    , score : Score
    , winner : Winner
    , private : PrivateGameState --not sent over the wire
    }


type alias ArchivedGame =
    { players : PlayerNames
    , winner : Winner
    , initialBoard : Maybe InitialBoard
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

        ResignRsp ({ gameState } as rec) ->
            ResignRsp { rec | gameState = updater gameState }

        AnotherGameRsp ({ gameState } as rec) ->
            AnotherGameRsp { rec | gameState = updater gameState }

        GameOverRsp ({ gameState } as rec) ->
            GameOverRsp { rec | gameState = updater gameState }

        _ ->
            message


type Choice
    = ChooseTableau Card
    | ChooseStock
    | SkipStock
    | Discard Card
    | SayUncle


type Message
    = NewReq
        { name : String
        , player : Player
        , publicType : PublicType
        , gamename : String
        , restoreState : Maybe GameState
        , maybeGameid : Maybe GameId
        }
    | NewRsp
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        , publicType : PublicType
        , gamename : String
        , gameState : GameState
        , wasRestored : Bool
        }
    | JoinReq
        { gameid : GameId
        , name : String
        , isRestore : Bool
        , inCrowd : Bool
        }
    | ReJoinReq
        { gameid : GameId
        , playerid : PlayerId
        }
    | JoinRsp
        { gameid : GameId
        , playerid : Maybe PlayerId
        , participant : Participant
        , gameState : GameState
        , wasRestored : Bool
        }
    | LeaveReq { playerid : PlayerId }
    | LeaveRsp { gameid : GameId, participant : Participant }
      -- Disallowed if Agog.WhichServer.allowGameState is False
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
    | ResignRsp
        { gameid : GameId
        , gameState : GameState
        , player : Player
        }
    | AnotherGameRsp
        { gameid : GameId
        , gameState : GameState
        , player : Player
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
    , watchers : Int
    , startTime : Posix
    , endTime : Posix
    }


messageToPlayer : Message -> Maybe Player
messageToPlayer message =
    case message of
        NewReq { player } ->
            Just player

        NewRsp { player } ->
            Just player

        ResignRsp { player } ->
            Just player

        AnotherGameRsp { player } ->
            Just player

        _ ->
            Nothing


messageToPlayerid : Message -> Maybe PlayerId
messageToPlayerid message =
    case message of
        NewRsp { playerid } ->
            Just playerid

        JoinRsp { playerid } ->
            playerid

        LeaveReq { playerid } ->
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

        LeaveRsp { gameid } ->
            Just gameid

        UpdateRsp { gameid } ->
            Just gameid

        PlayRsp { gameid } ->
            Just gameid

        ResignRsp { gameid } ->
            Just gameid

        AnotherGameRsp { gameid } ->
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
        , player : Player
        , publicType : PublicType
        , gamename : String
        , restoreState : Maybe String
        , maybeGameid : Maybe GameId
        }
    | NewRspLog
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        , publicType : PublicType
        , gamename : String
        , gameState : String
        , wasRestored : Bool
        }
    | JoinReqLog
        { gameid : GameId
        , name : String
        , isRestore : Bool
        , inCrowd : Bool
        }
    | RejoinReqLog
        { gameid : GameId
        , playerid : PlayerId
        }
    | JoinRspLog
        { gameid : GameId
        , playerid : Maybe PlayerId
        , participant : Participant
        , gameState : String
        , wasRestored : Bool
        }
    | LeaveReqLog { playerid : PlayerId }
    | LeaveRspLog { gameid : GameId, participant : Participant }
      -- Disallowed if Agog.WhichServer.allowGameState is False
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
    | ResignRspLog
        { gameid : GameId
        , gameState : String
        , player : Player
        }
    | AnotherGameRspLog
        { gameid : GameId
        , gameState : String
        , player : Player
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
    WebSocketFramework.Types.ServerState GameState Participant


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
    , whiteWon : String
    , blackWon : String
    , activeGames : String
    , totalConnections : String
    , totalPublicConnections : String
    , activeConnections : String
    }


statisticsKeys : StatisticsKeys
statisticsKeys =
    { finishedGames = "Finished Games"
    , totalMoves = "Finished Game Total Moves"
    , whiteWon = "White Won"
    , blackWon = "Black Won"
    , activeGames = "Active Games"
    , totalConnections = "Total Sessions"
    , totalPublicConnections = "Total Public Sessions"
    , activeConnections = "Active Sessions"
    }


statisticsKeyOrder : List (StatisticsKeys -> String)
statisticsKeyOrder =
    [ .finishedGames
    , .totalMoves
    , .whiteWon
    , .blackWon
    , .activeGames
    , .totalConnections
    , .totalPublicConnections
    , .activeConnections
    ]


type alias ServerInterface msg =
    WebSocketFramework.Types.ServerInterface GameState Participant Message msg


type alias ChatSettings msg =
    ElmChat.Settings msg


defaultGamename : String
defaultGamename =
    "default"


type alias NamedGame msg =
    { gamename : String
    , gameid : GameId
    , gameState : GameState
    , isLocal : Bool
    , serverUrl : String
    , otherPlayerid : PlayerId
    , player : Player
    , watcherName : Maybe String
    , playerid : PlayerId
    , isLive : Bool
    , yourWins : Int
    , archives : List ArchivedGame

    -- Not persistent
    , interfaceIsProxy : Bool
    , interface : ServerInterface msg
    }
