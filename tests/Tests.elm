module Tests exposing (all)

import Array exposing (Array)
import Cards exposing (Card(..), Face(..), Suit(..))
import Deck exposing (Deck, ShuffledDeck)
import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Maybe exposing (withDefault)
import Random exposing (Seed)
import SayUncle.Board as Board
import SayUncle.EncodeDecode as ED
import SayUncle.Types as Types
    exposing
        ( Board
        , Choice(..)
        , GameState
        , Message(..)
        , Participant(..)
        , Player
        , PlayerNames
        , PrivateGameState
        , PublicGame
        , PublicType(..)
        , RowCol
        , Score
        , State(..)
        , WinReason(..)
        , Winner(..)
        )
import Set exposing (Set)
import Test exposing (..)
import Time
import WebSocketFramework.Types exposing (Statistics)


log =
    Debug.log


enableLogging : Bool
enableLogging =
    False



--change to True to log JSON input & output results


maybeLog : String -> a -> a
maybeLog label value =
    if enableLogging then
        log label value

    else
        value


testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let
        numbers =
            List.map Debug.toString <| List.range 1 (List.length data)
    in
    List.map2 test data numbers


all : Test
all =
    Test.concat <|
        List.concat
            [ testMap protocolTest protocolData
            , testMap boardTest boardData
            , testMap gameStateTest gameStateData
            , testMap publicGameTest publicGameData
            ]


expectResult : Result String thing -> Result String thing -> Expectation
expectResult sb was =
    case maybeLog "  result" was of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false msg True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


rc : Int -> Int -> RowCol
rc x y =
    { row = x, col = y }


protocolTest : Message -> String -> Test
protocolTest message name =
    test ("protocolTest \"" ++ name ++ "\"")
        (\_ ->
            let
                pair =
                    maybeLog "protocolJson" <| ED.messageEncoderWithPrivate message
            in
            expectResult (Ok message) <| ED.messageDecoder pair
        )


protocolData : List Message
protocolData =
    [ NewReq
        { name = "Bill"
        , publicType = NotPublic
        , restoreState = Nothing
        , maybeGameid = Nothing
        }
    , NewReq
        { name = "Joe"
        , publicType = EntirelyPublic
        , restoreState = Just gameState1
        , maybeGameid = Just "Joe1"
        }
    , NewReq
        { name = "Joe"
        , publicType = PublicFor "Bill"
        , restoreState = Just gameState1
        , maybeGameid = Just "Joe2"
        }
    , NewRsp
        { gameid = "123"
        , playerid = "76"
        , player = 0
        , name = "Joe"
        , publicType = NotPublic
        , gameState = gameState1
        , wasRestored = False
        }
    , NewRsp
        { gameid = "123a"
        , playerid = "76b"
        , player = 1
        , name = "Joel"
        , publicType = EntirelyPublic
        , gameState = gameState2
        , wasRestored = True
        }
    , NewRsp
        { gameid = "123a"
        , playerid = "76b"
        , player = 2
        , name = "Joel"
        , publicType = PublicFor "Bill"
        , gameState = gameState2
        , wasRestored = False
        }
    , JoinReq
        { gameid = "123"
        , name = "Irving"
        , isRestore = False
        , inCrowd = False
        }
    , ReJoinReq
        { gameid = "123"
        , playerid = "76"
        }
    , JoinRsp
        { gameid = "123"
        , playerid = Just "77"
        , participant = PlayingParticipant 2
        , gameState = gameState2
        , wasRestored = False
        }
    , JoinRsp
        { gameid = "123"
        , playerid = Just "77"
        , participant = PlayingParticipant 3
        , gameState = gameState2
        , wasRestored = False
        }
    , JoinRsp
        { gameid = "123"
        , playerid = Nothing
        , participant = CrowdParticipant "Irving"
        , gameState = gameState2
        , wasRestored = True
        }
    , UpdateReq { playerid = "77" }
    , UpdateRsp
        { gameid = "123"
        , gameState = gameState1
        }
    , PlayReq
        { playerid = "77"
        , placement = ChooseNew
        }
    , PlayReq
        { playerid = "77"
        , placement = ChooseTableau (Card Hearts Ace)
        }
    , PlayReq
        { playerid = "78"
        , placement = ChooseStock
        }
    , PlayReq
        { playerid = "78"
        , placement = SkipStock
        }
    , PlayReq
        { playerid = "78"
        , placement = Discard (Card Spades Jack)
        }
    , PlayReq
        { playerid = "79"
        , placement = SayUncle
        }
    , AnotherGameRsp
        { playerid = "foo"
        , gameState = gameState1
        }
    , AnotherGameRsp
        { playerid = "foo"
        , gameState = gameState2
        }
    , GameOverRsp
        { gameid = "80"
        , gameState =
            { gameState1
                | winner =
                    SayUncleWinner { saidUncle = 1, won = 1 }
            }
        }
    , GameOverRsp
        { gameid = "80"
        , gameState = { gameState2 | winner = StockUsedWinner 1 }
        }
    , PublicGamesReq
        { subscribe = False
        , forName = ""
        , gameid = Nothing
        }
    , PublicGamesReq
        { subscribe = True
        , forName = "Bill"
        , gameid = Just "80"
        }
    , PublicGamesRsp
        { games = [] }
    , PublicGamesRsp
        { games =
            [ { publicGame = publicGame1
              , players = players1
              , watchers = 3
              , startTime = Time.millisToPosix 0
              , endTime = Time.millisToPosix 200
              }
            , { publicGame = publicGame2
              , players = players2
              , watchers = 3
              , startTime = Time.millisToPosix 0
              , endTime = Time.millisToPosix 200
              }
            ]
        }
    , PublicGamesUpdateRsp
        { added =
            [ { publicGame = publicGame1
              , players = players1
              , watchers = 1
              , startTime = Time.millisToPosix 0
              , endTime = Time.millisToPosix 100
              }
            , { publicGame = publicGame2
              , players = players2
              , watchers = 2
              , startTime = Time.millisToPosix 100
              , endTime = Time.millisToPosix 234
              }
            ]
        , removed = []
        }
    , PublicGamesUpdateRsp
        { added = []
        , removed = [ "foo", "bar" ]
        }
    , StatisticsReq { subscribe = True }
    , StatisticsReq { subscribe = False }
    , StatisticsRsp
        { statistics = Nothing
        , startTime = Nothing
        , updateTime = Nothing
        }
    , StatisticsRsp
        { statistics = Just <| Dict.fromList [ ( "foo", 1 ), ( "bar", 2 ) ]
        , startTime = Just 0
        , updateTime = Just 10
        }
    , ErrorRsp
        { request = "request"
        , text = "text"
        }
    , ChatReq
        { playerid = "77"
        , text = "text"
        }
    , ChatRsp
        { gameid = "123"
        , name = "Bob"
        , text = "text"
        }
    ]


expectString : String -> String -> Expectation
expectString sb was =
    Expect.equal sb was


publicGameTest : PublicGame -> String -> Test
publicGameTest game name =
    test ("publicGameTest \"" ++ name ++ "\"")
        (\_ ->
            let
                frameworkGame =
                    maybeLog "frameworkGame" <| ED.publicGameToFramework game
            in
            expectResult (Ok game) <|
                (ED.frameworkToPublicGame frameworkGame
                    |> Result.fromMaybe "bad conversion"
                )
        )


publicGameData : List PublicGame
publicGameData =
    [ publicGame1
    , publicGame2
    ]


boardTest : String -> String -> Test
boardTest encodedBoard name =
    test ("boardTest \"" ++ name ++ "\"")
        (\_ ->
            let
                board =
                    ED.stringToBoard encodedBoard

                boardString =
                    case board of
                        Nothing ->
                            ""

                        Just b ->
                            ED.boardToString b
            in
            expectString encodedBoard boardString
        )


tableau1 : Array (Maybe Card)
tableau1 =
    Array.fromList [ Nothing, Just <| Card Spades King, Just <| Card Clubs Ace ]


tableau2 : Array (Maybe Card)
tableau2 =
    Deck.fullDeck
        |> Deck.getCards
        |> List.map Just
        |> Array.fromList


seed : Seed
seed =
    Random.initialSeed 0


board1 : Board
board1 =
    Board.initial 2 seed
        |> Tuple.first


aceOfSpades : Card
aceOfSpades =
    Card Spades Ace


aosDefault : Maybe (Maybe Card) -> Card
aosDefault maybeMaybeCard =
    Maybe.withDefault Nothing maybeMaybeCard
        |> Maybe.withDefault aceOfSpades


board2 : Board
board2 =
    let
        { tableau, stock, hands } =
            board1

        c1 =
            aosDefault <|
                Array.get 0 tableau

        c2 =
            aosDefault <|
                Array.get 1 tableau

        ( ts, s2 ) =
            Deck.draw stock

        h1 =
            Maybe.withDefault [] <| Array.get 0 hands

        h2 =
            Maybe.withDefault [] <| Array.get 1 hands
    in
    { board1
        | tableau =
            Array.set 0 Nothing tableau
                |> Array.set 1 Nothing
        , stock = s2
        , turnedStock = Just ts
        , hands =
            Array.set 0 (c1 :: h1) hands
                |> Array.set 1 (c2 :: h2)
    }


stock1 : ShuffledDeck
stock1 =
    Deck.fullDeck


stock2 : ShuffledDeck
stock2 =
    Random.step Deck.randomDeck seed
        |> Tuple.first


boardData : List String
boardData =
    [ ED.boardToString board1
    , ED.boardToString board2
    ]


decodeValue : Decoder a -> Value -> Result String a
decodeValue decoder value =
    case JD.decodeValue decoder value of
        Ok a ->
            Ok a

        Err err ->
            Err <| JD.errorToString err


gameStateTest : GameState -> String -> Test
gameStateTest gameState name =
    test ("gameStateTest \"" ++ name ++ "\"")
        (\_ ->
            let
                value =
                    maybeLog "gameState" <| ED.encodeGameState True gameState
            in
            expectResult (Ok gameState) <|
                decodeValue ED.gameStateDecoder value
        )


players1 : PlayerNames
players1 =
    Dict.fromList [ ( 0, "Bill" ), ( 1, "Tom" ) ]


players2 : PlayerNames
players2 =
    Dict.fromList [ ( 0, "Larry" ), ( 2, "Moe" ), ( 3, "Curly" ) ]


score1 =
    { games = 1
    , points = Dict.fromList [ ( 0, 2 ) ]
    }


score2 =
    { games = 2
    , points = Dict.fromList [ ( 0, 2 ), ( 1, 1 ) ]
    }


privateGameState1 : PrivateGameState
privateGameState1 =
    Types.emptyPrivateGameState


privateGameState2 : PrivateGameState
privateGameState2 =
    { privateGameState1
        | subscribers =
            Set.fromList [ ( "s1", "1" ), ( "s2", "2" ), ( "s3", "3" ) ]
    }


privateGameState3 =
    { privateGameState2
        | verbose = Just True
        , statisticsSubscribers = Set.fromList [ "s1", "s2" ]
    }


privateGameState4 =
    { privateGameState3
        | verbose = Just False
        , statisticsChanged = True
        , startTime = Just 1
        , updateTime = Just 2
    }


gameState1 : GameState
gameState1 =
    { board = board1
    , players = players1
    , whoseTurn = 0
    , player = 1
    , state = TableauState
    , score = score1
    , winner = NoWinner
    , private = privateGameState1
    }


gameState2 =
    { board = board1
    , players = players1
    , whoseTurn = 0
    , player = 0
    , state = TurnStockState
    , score = score1
    , winner = SayUncleWinner { saidUncle = 0, won = 1 }
    , private = privateGameState2
    }


gameState3 =
    { gameState2
        | state = ChooseStockState
        , winner = StockUsedWinner 0
    }


gameState4 =
    { gameState3
        | state = DiscardState
        , private = privateGameState4
    }


gameState5 =
    { gameState3
        | state = ScoreState
    }


gameStateData : List GameState
gameStateData =
    [ gameState1
    , gameState2
    , gameState3
    , gameState4
    , gameState5
    ]


publicGame1 : PublicGame
publicGame1 =
    { gameid = "foo"
    , creator = "Bill"
    , player = 1
    , forName = Nothing
    }


publicGame2 : PublicGame
publicGame2 =
    { gameid = "bar"
    , creator = "Chris"
    , player = 2
    , forName = Just "Bill"
    }
