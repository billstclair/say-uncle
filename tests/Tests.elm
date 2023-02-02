module Tests exposing (all)

import Agog.Board as Board
import Agog.EncodeDecode as ED
import Agog.Types as Types
    exposing
        ( Board
        , Choice(..)
        , ChooseMoveOption(..)
        , Color(..)
        , GameState
        , HulkAfterJump(..)
        , Message(..)
        , MovesOrJumps(..)
        , OneMoveSequence(..)
        , Participant(..)
        , PieceType(..)
        , Player(..)
        , PlayerNames
        , PrivateGameState
        , PublicGame
        , PublicType(..)
        , RequestUndo(..)
        , RowCol
        , Score
        , UndoWhichJumps(..)
        , WinReason(..)
        , Winner(..)
        )
import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Maybe exposing (withDefault)
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
        , player = WhitePlayer
        , publicType = NotPublic
        , gamename = "Game 1"
        , restoreState = Nothing
        , maybeGameid = Nothing
        }
    , NewReq
        { name = "Joe"
        , player = BlackPlayer
        , publicType = EntirelyPublic
        , gamename = "Game 2"
        , restoreState = Just gameState1
        , maybeGameid = Just "Joe1"
        }
    , NewReq
        { name = "Joe"
        , player = WhitePlayer
        , publicType = PublicFor "Bill"
        , gamename = "Game 3"
        , restoreState = Just gameState1
        , maybeGameid = Just "Joe2"
        }
    , NewRsp
        { gameid = "123"
        , gamename = "Game 3"
        , playerid = "76"
        , player = BlackPlayer
        , name = "Joe"
        , publicType = NotPublic
        , gameState = gameState1
        , wasRestored = False
        }
    , NewRsp
        { gameid = "123a"
        , gamename = "Game 4"
        , playerid = "76b"
        , player = WhitePlayer
        , name = "Joel"
        , publicType = EntirelyPublic
        , gameState = gameState2
        , wasRestored = True
        }
    , NewRsp
        { gameid = "123a"
        , gamename = "Game 5"
        , playerid = "76b"
        , player = WhitePlayer
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
        , participant = PlayingParticipant WhitePlayer
        , gameState = gameState2
        , wasRestored = False
        }
    , JoinRsp
        { gameid = "123"
        , playerid = Just "77"
        , participant = PlayingParticipant BlackPlayer
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
    , LeaveReq { playerid = "77" }
    , LeaveRsp { gameid = "123", participant = PlayingParticipant WhitePlayer }
    , LeaveRsp { gameid = "123", participant = PlayingParticipant BlackPlayer }
    , LeaveRsp { gameid = "123", participant = CrowdParticipant "Wilfred" }
    , UpdateReq { playerid = "77" }
    , UpdateRsp
        { gameid = "123"
        , gameState = gameState1
        }
    , PlayReq
        { playerid = "77"
        , placement = ChoosePiece <| rc 0 0
        }
    , PlayReq
        { playerid = "77"
        , placement = ChoosePiece <| rc 1 2
        }
    , PlayReq
        { playerid = "78"
        , placement = ChooseMove (rc 2 3) NoOption
        }
    , PlayReq
        { playerid = "78"
        , placement = ChooseMove (rc 3 4) CorruptJumped
        }
    , PlayReq
        { playerid = "78"
        , placement = ChooseMove (rc 4 5) (MakeHulk <| rc 3 4)
        }
    , PlayReq
        { playerid = "79"
        , placement = ChooseUndoJump UndoOneJump
        }
    , PlayReq
        { playerid = "79"
        , placement = ChooseUndoJump UndoAllJumps
        }
    , PlayReq
        { playerid = "79"
        , placement = ChooseRequestUndo "Please"
        }
    , PlayReq
        { playerid = "80"
        , placement = ChooseAcceptUndo
        }
    , PlayReq
        { playerid = "80"
        , placement = ChooseDenyUndo "Nope"
        }
    , PlayReq
        { playerid = "79"
        , placement = ChooseResign WhitePlayer
        }
    , PlayReq
        { playerid = "79"
        , placement = ChooseResign BlackPlayer
        }
    , PlayReq
        { playerid = "80"
        , placement = ChooseNew WhitePlayer
        }
    , PlayReq
        { playerid = "80"
        , placement = ChooseNew BlackPlayer
        }
    , PlayRsp
        { gameid = "77"
        , gameState = gameState1
        }
    , PlayRsp
        { gameid = "78"
        , gameState = gameState2
        }
    , PlayRsp
        { gameid = "78"
        , gameState = gameState4
        }
    , ResignRsp
        { gameid = "79"
        , gameState = { gameState3 | winner = WhiteWinner WinByCapture }
        , player = WhitePlayer
        }
    , ResignRsp
        { gameid = "79"
        , gameState = { gameState3 | winner = BlackWinner WinBySanctum }
        , player = BlackPlayer
        }
    , AnotherGameRsp
        { gameid = "80"
        , gameState = gameState1
        , player = WhitePlayer
        }
    , AnotherGameRsp
        { gameid = "80"
        , gameState = gameState2
        , player = BlackPlayer
        }
    , GameOverRsp
        { gameid = "80"
        , gameState = { gameState1 | winner = WhiteWinner WinByImmobilization }
        }
    , GameOverRsp
        { gameid = "80"
        , gameState = { gameState2 | winner = BlackWinner WinByResignation }
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
              , players = PlayerNames "Bill" "Joe"
              , watchers = 3
              , moves = 4
              , startTime = Time.millisToPosix 0
              , endTime = Time.millisToPosix 200
              }
            , { publicGame = publicGame2
              , players = PlayerNames "Bill" "Joe"
              , watchers = 3
              , moves = 4
              , startTime = Time.millisToPosix 0
              , endTime = Time.millisToPosix 200
              }
            ]
        }
    , PublicGamesUpdateRsp
        { added =
            [ { publicGame = publicGame1
              , players = PlayerNames "Bill" "Joe"
              , watchers = 1
              , moves = 2
              , startTime = Time.millisToPosix 0
              , endTime = Time.millisToPosix 100
              }
            , { publicGame = publicGame2
              , players = PlayerNames "Bob" "Ben"
              , watchers = 2
              , moves = 3
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
                            ED.newBoardToString b
            in
            expectString encodedBoard boardString
        )


board1String =
    "--gggggj|---ggggg|G----ggg|GG---ggg|GGG-G-gg|GGG----g|GGGG----|JGGGGG--"


board2String =
    "--gggggj|---ggggg|G---gggg|GG----gg|GGG--ggg|GGG-G--g|GGGGG---|JGGGGG--"


decodeBoard : String -> Board
decodeBoard string =
    ED.stringToBoard string
        |> Maybe.withDefault Board.empty


board1 =
    decodeBoard board1String


board2 =
    decodeBoard board2String


boardData : List String
boardData =
    [ board1String
    , board2String
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


players1 =
    PlayerNames "Billy Bob" "Bobby Sue"


players2 =
    PlayerNames "Joe" "Random"


score1 =
    Score 0 1 2


score2 =
    Score 4 5 6


privateGameState1 =
    Types.emptyPrivateGameState


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


move1 =
    { piece = { color = WhiteColor, pieceType = Golem }
    , isUnique = True
    , sequence = OneSlide { from = rc 0 0, to = rc 2 0, makeHulk = Nothing }
    , winner = NoWinner
    , time = Time.millisToPosix 0
    }


move2 =
    { piece = { color = BlackColor, pieceType = Journeyman }
    , isUnique = True
    , sequence =
        OneJumpSequence
            [ { from = rc 0 0
              , over = rc 0 1
              , to = rc 0 2
              , hulkAfterJump = NoHulkAfterJump
              }
            ]
    , winner = NoWinner
    , time = Time.millisToPosix 0
    }


gameState1 =
    { newBoard = board1
    , initialBoard = Nothing
    , moves =
        [ { piece = { color = WhiteColor, pieceType = Golem }
          , isUnique = True
          , sequence =
                OneSlide
                    { from = rc 0 0
                    , to = rc 0 1
                    , makeHulk = Nothing
                    }
          , winner = NoWinner
          , time = Time.millisToPosix 0
          }
        ]
    , players = players1
    , whoseTurn = WhitePlayer
    , selected = Nothing
    , jumperLocations = []
    , legalMoves = NoMoves
    , undoStates = []
    , jumps = []
    , score = score1
    , winner = NoWinner
    , requestUndo = NoRequestUndo
    , testMode = Nothing
    , testModeInitialState = Nothing
    , private = privateGameState1
    }


gameState2 =
    { newBoard = board1
    , initialBoard = Just { board = board2, whoseTurn = WhitePlayer }
    , moves =
        [ { piece = { color = BlackColor, pieceType = Golem }
          , isUnique = False
          , sequence =
                OneJumpSequence
                    [ { from = rc 0 0
                      , over = rc 0 1
                      , to = rc 0 2
                      , hulkAfterJump = CorruptAfterJump
                      }
                    , { from = rc 0 2
                      , over = rc 1 2
                      , to = rc 2 2
                      , hulkAfterJump = NoHulkAfterJump
                      }
                    ]
          , winner = WhiteWinner WinByCapture
          , time = Time.millisToPosix 100 --must be a multiple of 100
          }
        ]
    , players = players1
    , whoseTurn = BlackPlayer
    , selected = Nothing
    , jumperLocations = []
    , legalMoves = NoMoves
    , undoStates = []
    , jumps = []
    , score = score1
    , winner = WhiteWinner WinByCapture
    , requestUndo = NoRequestUndo
    , testMode = Nothing
    , testModeInitialState = Nothing
    , private = privateGameState2
    }


gameState3 =
    { gameState2
        | selected = Just <| rc 0 0
        , jumperLocations = [ rc 0 0, rc 1 1 ]
        , legalMoves = Moves [ rc 2 2, rc 3 3 ]
    }


gameState4 =
    { gameState3 | private = privateGameState4 }


gameStateData : List GameState
gameStateData =
    [ gameState1
    , gameState2
    , gameState3
    ]


publicGame1 : PublicGame
publicGame1 =
    { gameid = "foo"
    , creator = "Bill"
    , player = WhitePlayer
    , forName = Nothing
    }


publicGame2 : PublicGame
publicGame2 =
    { gameid = "bar"
    , creator = "Chris"
    , player = BlackPlayer
    , forName = Just "Bill"
    }
