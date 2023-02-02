---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- Zephyrnot JSON encoders and decoders
-- Copyright (c) 2019-2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Agog.EncodeDecode exposing
    ( archivedGameDecoder
    , decodeSavedModel
    , defaultOneMove
    , encodeArchivedGame
    , encodeGameState
    , encodeMessageForLog
    , encodeMoves
    , encodeNamedGame
    , encodeOneMove
    , encodeOneMoveList
    , encodeParticipant
    , encodePublicGameAndPlayers
    , encodeRowCol
    , encodeSavedModel
    , encodeWithTimestamp
    , frameworkToPublicGame
    , gameStateDecoder
    , maybeOneMoveSequenceToString
    , maybeOneMoveToPrettyString
    , maybeOneMoveToString
    , messageDecoder
    , messageEncoder
    , messageEncoderWithPrivate
    , messageToLogMessage
    , movesDecoder
    , namedGameDecoder
    , newBoardToString
    , oneMoveDecoder
    , oneMoveListDecoder
    , oneMoveSequenceToString
    , oneMoveToPrettyString
    , oneMoveToString
    , oneMovesToString
    , participantDecoder
    , pieceToString
    , publicGameAndPlayersDecoder
    , publicGameToFramework
    , rowColDecoder
    , stringToBoard
    , stringToFromSlashOver
    , stringToOneMove
    , stringToOneMoveSequence
    , stringToPiece
    , withTimestampDecoder
    )

import Agog.Board as Board exposing (rc)
import Agog.Types as Types
    exposing
        ( ArchivedGame
        , Board
        , ChatSettings
        , Choice(..)
        , ChooseMoveOption(..)
        , Color(..)
        , GameState
        , HulkAfterJump(..)
        , InitialBoard
        , JumpSequence
        , Message(..)
        , MessageForLog(..)
        , MovesOrJumps(..)
        , NamedGame
        , OneCorruptibleJump
        , OneJump
        , OneMove
        , OneMoveSequence(..)
        , OneSlideRecord
        , Page(..)
        , Participant(..)
        , Piece
        , PieceType(..)
        , Player(..)
        , PlayerNames
        , PrivateGameState
        , PublicGame
        , PublicGameAndPlayers
        , PublicType(..)
        , RequestUndo(..)
        , RotateBoard(..)
        , RowCol
        , SavedModel
        , Score
        , ServerInterface
        , Settings
        , Socket
        , StyleType(..)
        , TestMode
        , TestModeInitialState
        , UndoState
        , UndoWhichJumps(..)
        , WinReason(..)
        , Winner(..)
        )
import Agog.WhichServer as WhichServer
import Array exposing (Array)
import Dict exposing (Dict)
import ElmChat
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Set exposing (Set)
import Time exposing (Posix)
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.EncodeDecode as WSFED
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , Plist
        , ReqRsp(..)
        , ServerState
        , Statistics
        )


encodeMoves : List String -> Value
encodeMoves moves =
    moves
        |> List.intersperse ","
        |> String.concat
        |> JE.string


movesDecoder : Decoder (List String)
movesDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (String.split "," >> List.filter ((/=) "") >> JD.succeed)
        , JD.list JD.string --backward compatibility
        ]


encodeStyleType : StyleType -> Value
encodeStyleType styleType =
    case styleType of
        DarkStyle ->
            JE.string "DarkStyle"

        _ ->
            JE.string "LightStyle"


styleTypeDecoder : Decoder StyleType
styleTypeDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "LightStyle" ->
                        JD.succeed LightStyle

                    "DarkStyle" ->
                        JD.succeed DarkStyle

                    _ ->
                        JD.fail "Unknown StyleType name."
            )


encodeRotateBoard : RotateBoard -> Value
encodeRotateBoard rotate =
    JE.string <|
        case rotate of
            RotateWhiteDown ->
                "RotateWhiteDown"

            RotatePlayerDown ->
                "RotatePlayerDown"


boardRotateDecoder : Decoder RotateBoard
boardRotateDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "RotateWhiteDown" ->
                        JD.succeed RotateWhiteDown

                    "RotatePlayerDown" ->
                        JD.succeed RotatePlayerDown

                    _ ->
                        JD.fail <| "Unknown RotateBoard value: " ++ s
            )


encodeTestMode : TestMode -> Value
encodeTestMode { piece, clear } =
    if not clear then
        encodePiece piece

    else
        JE.object
            [ ( "piece", encodePiece piece )
            , ( "clear", JE.bool clear )
            ]


testModeDecoder : Decoder TestMode
testModeDecoder =
    JD.oneOf
        [ pieceDecoder
            |> JD.andThen
                (\piece ->
                    JD.succeed
                        { piece = piece
                        , clear = False
                        }
                )
        , JD.succeed TestMode
            |> required "piece" pieceDecoder
            |> required "clear" JD.bool
        ]


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "gamename", JE.string model.gamename )
        , ( "gameGamename", JE.string model.gameGamename )
        , ( "page", encodePage model.page )
        , ( "chooseFirst", encodePlayer model.chooseFirst )
        , ( "lastTestMode", encodeMaybe encodeTestMode model.lastTestMode )
        , ( "gameid", JE.string model.gameid )
        , ( "settings", encodeSettings model.settings )
        , ( "styleType", encodeStyleType model.styleType )
        , ( "rotate", encodeRotateBoard model.rotate )
        , ( "notificationsEnabled", JE.bool model.notificationsEnabled )
        , ( "soundEnabled", JE.bool model.soundEnabled )
        , ( "requestUndoMessage", JE.string model.requestUndoMessage )
        , ( "denyUndoMessage", JE.string model.denyUndoMessage )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "gamename" JD.string Types.defaultGamename
        |> optional "gameGamename" JD.string Types.defaultGamename
        |> optional "page" pageDecoder MainPage
        |> required "chooseFirst" playerDecoder
        |> optional "lastTestMode" (JD.nullable testModeDecoder) Nothing
        |> optional "gameid" JD.string ""
        |> optional "settings" settingsDecoder Types.emptySettings
        |> optional "styleType" styleTypeDecoder LightStyle
        |> optional "rotate" boardRotateDecoder RotateWhiteDown
        |> optional "notificationsEnabled" JD.bool False
        |> optional "soundEnabled" JD.bool False
        |> optional "requestUndoMessage" JD.string ""
        |> optional "denyUndoMessage" JD.string ""


encodePage : Page -> Value
encodePage page =
    JE.string <|
        case page of
            MainPage ->
                "MainPage"

            RulesPage ->
                "RulesPage"

            InstructionsPage ->
                "InstructionsPage"

            PublicPage ->
                "PublicPage"

            MovesPage ->
                "MovesPage"

            StatisticsPage ->
                "StatisticsPage"


pageDecoder : Decoder Page
pageDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "MainPage" ->
                        JD.succeed MainPage

                    "RulesPage" ->
                        JD.succeed RulesPage

                    "InstructionsPage" ->
                        JD.succeed InstructionsPage

                    "PublicPage" ->
                        JD.succeed PublicPage

                    "MovesPage" ->
                        JD.succeed MovesPage

                    "StatisticsPage" ->
                        JD.succeed StatisticsPage

                    _ ->
                        JD.fail <| "Unknown page: " ++ s
            )


encodePlayer : Player -> Value
encodePlayer player =
    case player of
        WhitePlayer ->
            JE.string "White"

        BlackPlayer ->
            JE.string "Black"


playerDecoder : Decoder Player
playerDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "White" ->
                        JD.succeed WhitePlayer

                    "Black" ->
                        JD.succeed BlackPlayer

                    _ ->
                        JD.fail <| "Unknown player: " ++ s
            )


encodeParticipant : Participant -> Value
encodeParticipant participant =
    case participant of
        PlayingParticipant player ->
            JE.object [ ( "PlayingParticipant", encodePlayer player ) ]

        CrowdParticipant name ->
            JE.object [ ( "CrowdParticipant", JE.string name ) ]


participantDecoder : Decoder Participant
participantDecoder =
    JD.oneOf
        [ JD.succeed PlayingParticipant
            |> required "PlayingParticipant" playerDecoder
        , JD.succeed CrowdParticipant
            |> required "CrowdParticipant" JD.string
        ]


winReasonToString : WinReason -> String
winReasonToString reason =
    case reason of
        WinByCapture ->
            "C"

        WinBySanctum ->
            "S"

        WinByImmobilization ->
            "I"

        WinByResignation ->
            "R"


stringToWinReason : String -> Maybe WinReason
stringToWinReason string =
    case string of
        "C" ->
            Just WinByCapture

        "S" ->
            Just WinBySanctum

        "I" ->
            Just WinByImmobilization

        "R" ->
            Just WinByResignation

        _ ->
            Nothing


encodeWinReason : WinReason -> Value
encodeWinReason reason =
    JE.string <| winReasonToString reason


winReasonDecoder : Decoder WinReason
winReasonDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case stringToWinReason s of
                    Nothing ->
                        JD.fail <| "Unknown WinReason string: " ++ s

                    Just reason ->
                        JD.succeed reason
            )


winnerToString : Winner -> String
winnerToString winner =
    case winner of
        NoWinner ->
            "N"

        WhiteWinner reason ->
            "W" ++ winReasonToString reason

        BlackWinner reason ->
            "B" ++ winReasonToString reason


stringToWinner : String -> Maybe Winner
stringToWinner string =
    if string == "N" then
        Just NoWinner

    else
        case String.dropLeft 1 string |> stringToWinReason of
            Nothing ->
                Nothing

            Just reason ->
                case String.left 1 string of
                    "W" ->
                        Just <| WhiteWinner reason

                    "B" ->
                        Just <| BlackWinner reason

                    _ ->
                        Nothing


encodeWinner : Winner -> Value
encodeWinner winner =
    winnerToString winner |> JE.string


winnerDecoder : Decoder Winner
winnerDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    case stringToWinner s of
                        Nothing ->
                            JD.fail <| "Not a winner string: " ++ s

                        Just winner ->
                            JD.succeed winner
                )
        , oldWinnerDecoder
        ]


oldWinnerDecoder : Decoder Winner
oldWinnerDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if "NoWinner" == s then
                        JD.succeed NoWinner

                    else
                        JD.fail <| "Unknown win type: " ++ s
                )
        , JD.field "WhiteWinner" winReasonDecoder
            |> JD.andThen (\reason -> WhiteWinner reason |> JD.succeed)
        , JD.field "BlackWinner" winReasonDecoder
            |> JD.andThen (\reason -> BlackWinner reason |> JD.succeed)
        ]


encodeIntPair : ( Int, Int ) -> Value
encodeIntPair ( x, y ) =
    JE.list JE.int [ x, y ]


intPairDecoder : Decoder ( Int, Int )
intPairDecoder =
    JD.list JD.int
        |> JD.andThen
            (\list ->
                case list of
                    [ rowidx, colidx ] ->
                        JD.succeed ( rowidx, colidx )

                    _ ->
                        JD.fail "Wrong length Int pair"
            )


boolToString : Bool -> String
boolToString bool =
    if bool then
        "0"

    else
        "-"


stringToBool : String -> Bool
stringToBool string =
    string == "0"


rowToString : Array Bool -> String
rowToString row =
    Array.toList row
        |> List.map boolToString
        |> String.concat


stringToRow : String -> Array Bool
stringToRow string =
    String.toList string
        |> List.map String.fromChar
        |> List.map stringToBool
        |> Array.fromList


pieceToPrettyString : Piece -> String
pieceToPrettyString { pieceType } =
    case pieceType of
        Golem ->
            ""

        Hulk ->
            "H"

        CorruptedHulk ->
            "C"

        Journeyman ->
            "J"

        NoPiece ->
            "-"


pieceToString : Piece -> String
pieceToString { color, pieceType } =
    let
        letter =
            case pieceType of
                Golem ->
                    "G"

                Hulk ->
                    "H"

                CorruptedHulk ->
                    "C"

                Journeyman ->
                    "J"

                NoPiece ->
                    "-"
    in
    case color of
        WhiteColor ->
            letter

        BlackColor ->
            String.toLower letter


stringToPiece : String -> Piece
stringToPiece string =
    let
        upper =
            String.toUpper string

        pieceType =
            case upper of
                "G" ->
                    Golem

                "H" ->
                    Hulk

                "C" ->
                    CorruptedHulk

                "J" ->
                    Journeyman

                _ ->
                    NoPiece

        color =
            case pieceType of
                NoPiece ->
                    WhiteColor

                _ ->
                    if upper == string then
                        WhiteColor

                    else
                        BlackColor
    in
    { color = color
    , pieceType = pieceType
    }


encodePiece : Piece -> Value
encodePiece piece =
    JE.string <| pieceToString piece


pieceDecoder : Decoder Piece
pieceDecoder =
    JD.string
        |> JD.andThen
            (stringToPiece >> JD.succeed)


newRowToString : Array Piece -> String
newRowToString row =
    Array.toList row
        |> List.map pieceToString
        |> String.concat


stringToNewRow : String -> Array Piece
stringToNewRow string =
    String.toList string
        |> List.map String.fromChar
        |> List.map stringToPiece
        |> Array.fromList


newBoardToString : Board -> String
newBoardToString board =
    Array.toList board
        |> List.map newRowToString
        |> List.intersperse "|"
        |> String.concat


stringToBoard : String -> Maybe Board
stringToBoard string =
    if String.length string /= 8 * 8 + 7 then
        Nothing

    else
        let
            rows =
                [ String.slice 0 8 string
                , String.slice 9 17 string
                , String.slice 18 26 string
                , String.slice 27 35 string
                , String.slice 36 44 string
                , String.slice 45 53 string
                , String.slice 54 62 string
                , String.slice 63 71 string
                ]
        in
        rows
            |> List.map stringToNewRow
            |> Array.fromList
            |> Just


encodeBoard : Board -> Value
encodeBoard board =
    JE.string <| newBoardToString board


newBoardDecoder : Decoder Board
newBoardDecoder =
    JD.string
        |> JD.andThen
            (\string ->
                case stringToBoard string of
                    Nothing ->
                        JD.fail "Invalid board string."

                    Just board ->
                        JD.succeed board
            )


encodeScore : Score -> Value
encodeScore score =
    JE.object
        [ ( "games", JE.int score.games )
        , ( "whiteWins", JE.int score.whiteWins )
        , ( "blackWins", JE.int score.blackWins )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    JD.succeed Score
        |> required "games" JD.int
        |> required "whiteWins" JD.int
        |> required "blackWins" JD.int


encodeSettings : Settings -> Value
encodeSettings { name, isPublic, forName, hideTitle } =
    JE.object
        [ ( "name", JE.string name )
        , ( "isPublic", JE.bool isPublic )
        , ( "forName", JE.string forName )
        , ( "hideTitle", JE.bool hideTitle )
        ]


settingsDecoder : Decoder Settings
settingsDecoder =
    JD.succeed Settings
        |> required "name" JD.string
        |> optional "isPublic" JD.bool False
        |> optional "forName" JD.string ""
        |> required "hideTitle" JD.bool



---
--- Messages
---


encodePlayerNames : PlayerNames -> Value
encodePlayerNames { white, black } =
    JE.object
        [ ( "white", JE.string white )
        , ( "black", JE.string black )
        ]


playerNamesDecoder : Decoder PlayerNames
playerNamesDecoder =
    JD.succeed PlayerNames
        |> required "white" JD.string
        |> required "black" JD.string


encodePrivateGameState : PrivateGameState -> Value
encodePrivateGameState { verbose, subscribers, statisticsSubscribers, statisticsChanged, startTime, updateTime } =
    List.concat
        [ case verbose of
            Nothing ->
                []

            Just v ->
                [ ( "verbose", JE.bool v ) ]
        , case Set.toList subscribers of
            [] ->
                []

            list ->
                [ ( "subscribers", JE.list encodeSubscriberPair list ) ]
        , case Set.toList statisticsSubscribers of
            [] ->
                []

            list ->
                [ ( "statisticsSubscribers", JE.list JE.string list ) ]
        , if not statisticsChanged then
            []

          else
            [ ( "statisticsChanged", JE.bool True ) ]
        , case startTime of
            Nothing ->
                []

            Just time ->
                [ ( "startTime", JE.int time ) ]
        , case updateTime of
            Nothing ->
                []

            Just time ->
                [ ( "updateTime", JE.int time ) ]
        ]
        |> JE.object


encodeSubscriberPair : ( Socket, String ) -> Value
encodeSubscriberPair ( socket, forName ) =
    JE.list identity [ JE.string socket, JE.string forName ]


subscriberPairDecoder : Decoder ( Socket, String )
subscriberPairDecoder =
    JD.list JD.string
        |> JD.andThen
            (\list ->
                case list of
                    [ socket, forName ] ->
                        JD.succeed ( socket, forName )

                    _ ->
                        JD.fail "Not a two-element list"
            )


subscribersListDecoder : Decoder (List ( String, String ))
subscribersListDecoder =
    JD.list subscriberPairDecoder


subscribersDecoder : Decoder (Set ( String, String ))
subscribersDecoder =
    subscribersListDecoder
        |> JD.andThen (Set.fromList >> JD.succeed)


socketSetDecoder : Decoder (Set Socket)
socketSetDecoder =
    JD.list JD.string
        |> JD.andThen (Set.fromList >> JD.succeed)


privateGameStateDecoder : Decoder PrivateGameState
privateGameStateDecoder =
    JD.succeed PrivateGameState
        |> optional "verbose" (JD.nullable JD.bool) Nothing
        |> optional "subscribers" subscribersDecoder Set.empty
        |> optional "statisticsSubscribers" socketSetDecoder Set.empty
        |> optional "statisticsChanged" JD.bool False
        |> optional "startTime" (JD.nullable JD.int) Nothing
        |> optional "updateTime" (JD.nullable JD.int) Nothing


encodeOneMoveSequence : OneMoveSequence -> Value
encodeOneMoveSequence oneMoveSequence =
    case oneMoveSequence of
        OneResign ->
            JE.string "resign"

        OneSlide { from, to, makeHulk } ->
            JE.object
                ([ ( "from", encodeRowCol from )
                 , ( "to", encodeRowCol to )
                 ]
                    ++ (if makeHulk == Nothing then
                            []

                        else
                            [ ( "makeHulk", encodeMaybe encodeRowCol makeHulk ) ]
                       )
                )

        OneJumpSequence jumps ->
            JE.list encodeOneCorruptibleJump jumps


oneMoveSequenceDecoder : Decoder OneMoveSequence
oneMoveSequenceDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if s == "resign" then
                        JD.succeed OneResign

                    else
                        JD.fail <| "Unknown OneMoveSequence: " ++ s
                )
        , JD.succeed
            (\from to makeHulk ->
                OneSlide { from = from, to = to, makeHulk = makeHulk }
            )
            |> required "from" rowColDecoder
            |> required "to" rowColDecoder
            |> optional "makeHulk" (JD.nullable rowColDecoder) Nothing
        , JD.list oneCorruptibleJumpDecoder
            |> JD.andThen (OneJumpSequence >> JD.succeed)
        ]


oneMovesToString : List OneMove -> String
oneMovesToString moves =
    List.map oneMoveToString moves
        |> String.join ","


maybeOneMoveToString : Maybe OneMove -> Maybe String
maybeOneMoveToString maybeMove =
    case maybeMove of
        Nothing ->
            Nothing

        Just move ->
            Just <| oneMoveToString move


posixToString : Posix -> String
posixToString posix =
    Time.posixToMillis posix
        |> String.fromInt


stringToPosix : String -> Maybe Posix
stringToPosix string =
    case String.toInt string of
        Nothing ->
            Nothing

        Just int ->
            Just <| Time.millisToPosix int


maybeOneMoveToPrettyString : Maybe OneMove -> Maybe String
maybeOneMoveToPrettyString maybeOneMove =
    case maybeOneMove of
        Nothing ->
            Nothing

        Just oneMove ->
            Just <| oneMoveToPrettyString oneMove


oneMoveToPrettyString : OneMove -> String
oneMoveToPrettyString { piece, isUnique, sequence, winner } =
    let
        pieceString =
            pieceToPrettyString piece

        sequenceString =
            oneMoveSequenceToPrettyString isUnique sequence
    in
    pieceString ++ sequenceString


unsharedRowOrCol : RowCol -> RowCol -> String
unsharedRowOrCol from to =
    if from.row == to.row then
        Board.colToString from.col

    else if from.col == to.col then
        Board.rowToString from.row

    else
        Board.rowColToString from


oneMoveSequenceToPrettyString : Bool -> OneMoveSequence -> String
oneMoveSequenceToPrettyString isUnique sequence =
    case sequence of
        OneResign ->
            "resign"

        OneSlide { from, to, makeHulk } ->
            let
                fromString =
                    if isUnique then
                        ""

                    else
                        unsharedRowOrCol from to

                toString =
                    Board.rowColToString to

                hulkString =
                    case makeHulk of
                        Nothing ->
                            ""

                        Just h ->
                            " " ++ Board.rowColToString h ++ "=H"
            in
            fromString ++ toString ++ hulkString

        OneJumpSequence jumps ->
            case jumps of
                [] ->
                    ""

                firstJump :: _ ->
                    let
                        mapper list res =
                            case list of
                                [] ->
                                    res

                                head :: tail ->
                                    let
                                        { from, over, to, hulkAfterJump } =
                                            head

                                        ( x, eq ) =
                                            case hulkAfterJump of
                                                NoHulkAfterJump ->
                                                    ( "x", "" )

                                                CorruptAfterJump ->
                                                    ( "+", "" )

                                                MakeHulkAfterJump hulkPos ->
                                                    ( "x"
                                                    , " " ++ Board.rowColToString hulkPos ++ "=H"
                                                    )
                                    in
                                    mapper tail <|
                                        (x ++ Board.rowColToString to ++ eq)
                                            :: res

                        fromString =
                            unsharedRowOrCol firstJump.from firstJump.to
                    in
                    mapper jumps [ fromString ]
                        |> List.reverse
                        |> String.concat


oneMoveToString : OneMove -> String
oneMoveToString { piece, isUnique, sequence, winner, time } =
    let
        uniqueMarker =
            if isUnique then
                ""

            else
                "n"

        pieceString =
            pieceToString piece

        sequenceString =
            oneMoveSequenceToString sequence

        winString =
            case winner of
                NoWinner ->
                    ""

                _ ->
                    "%" ++ winnerToString winner

        posixString =
            if time == Types.posixZero then
                ""

            else
                ":" ++ posixToString time
    in
    uniqueMarker ++ pieceString ++ sequenceString ++ winString ++ posixString


maybeOneMoveSequenceToString : Maybe OneMoveSequence -> Maybe String
maybeOneMoveSequenceToString maybeSequence =
    case maybeSequence of
        Nothing ->
            Nothing

        Just sequence ->
            Just <| oneMoveSequenceToString sequence


oneMoveSequenceToString : OneMoveSequence -> String
oneMoveSequenceToString sequence =
    case sequence of
        OneResign ->
            "resign"

        OneSlide { from, to, makeHulk } ->
            Board.rowColToString from
                ++ "-"
                ++ Board.rowColToString to
                ++ (case makeHulk of
                        Nothing ->
                            ""

                        Just rc ->
                            "=" ++ Board.rowColToString rc
                   )

        OneJumpSequence jumps ->
            case jumps of
                [] ->
                    ""

                { from } :: _ ->
                    let
                        mapper list res =
                            case list of
                                [] ->
                                    res

                                head :: tail ->
                                    let
                                        { over, to, hulkAfterJump } =
                                            head

                                        from2 =
                                            head.from

                                        ( x, eq ) =
                                            case hulkAfterJump of
                                                NoHulkAfterJump ->
                                                    ( "x", "" )

                                                CorruptAfterJump ->
                                                    ( "+", "" )

                                                MakeHulkAfterJump hulkPos ->
                                                    ( "x", "=" ++ Board.rowColToString hulkPos )

                                        slashOver =
                                            if Just over == locBetween from2 to then
                                                ""

                                            else
                                                "/" ++ Board.rowColToString over
                                    in
                                    mapper tail <|
                                        (slashOver ++ x ++ Board.rowColToString to ++ eq)
                                            :: res
                    in
                    mapper jumps [ Board.rowColToString from ]
                        |> List.reverse
                        |> String.concat


stringToOneMove : String -> Maybe OneMove
stringToOneMove string =
    case String.split ":" string of
        [ move, timestr ] ->
            case stringToPosix timestr of
                Nothing ->
                    Nothing

                Just time ->
                    stringToOneMoveInternal move time

        [ move ] ->
            stringToOneMoveInternal move Types.posixZero

        _ ->
            Nothing


stringToOneMoveInternal : String -> Posix -> Maybe OneMove
stringToOneMoveInternal string time =
    let
        n =
            String.left 1 string

        ( isUnique, s, ps ) =
            if n == "n" then
                let
                    s2 =
                        String.dropLeft 1 string
                in
                ( False, String.dropLeft 1 s2, String.left 1 s2 )

            else
                ( True, String.dropLeft 1 string, String.left 1 string )

        piece =
            stringToPiece ps
    in
    case piece.pieceType of
        NoPiece ->
            Nothing

        _ ->
            let
                computeSequence ss winner =
                    case stringToOneMoveSequence ss of
                        Nothing ->
                            Nothing

                        Just sequence ->
                            Just
                                { piece = piece
                                , isUnique = isUnique
                                , sequence = sequence
                                , winner = winner
                                , time = time
                                }
            in
            case String.split "%" s of
                [ ss ] ->
                    computeSequence ss NoWinner

                [ ss, winString ] ->
                    case stringToWinner winString of
                        Nothing ->
                            Nothing

                        Just winner ->
                            computeSequence ss winner

                _ ->
                    Nothing


locBetween : RowCol -> RowCol -> Maybe RowCol
locBetween rc1 rc2 =
    let
        ( r1, c1 ) =
            ( rc1.row, rc1.col )

        ( r2, c2 ) =
            ( rc2.row, rc2.col )
    in
    if r1 == r2 then
        if c1 < c2 && c1 + 2 == c2 then
            Just <| rc r1 (c1 + 1)

        else if c1 > c2 && c1 - 2 == c2 then
            Just <| rc r1 (c1 - 1)

        else
            Nothing

    else if c1 == c2 then
        if r1 < r2 && r1 + 2 == r2 then
            Just <| rc (r1 + 1) c1

        else if r1 > r2 && r1 - 2 == r2 then
            Just <| rc (r1 - 1) c1

        else
            Nothing

    else
        Nothing


stringToOneMoveSequence : String -> Maybe OneMoveSequence
stringToOneMoveSequence string =
    if string == "resign" then
        Just OneResign

    else
        let
            jumps =
                String.split "x" string

            corruptingJumps =
                List.map (String.split "+") jumps
        in
        if
            (List.head jumps == Just string)
                && (List.head corruptingJumps == Just jumps)
        then
            -- It's a move
            case stringToOneSlideRecord string of
                Nothing ->
                    Nothing

                Just oneSlideRecord ->
                    Just <| OneSlide oneSlideRecord

        else
            -- It's a jump sequence
            case listOfStringListsToOneJumpSequence corruptingJumps of
                Nothing ->
                    Nothing

                Just oneJumpSequence ->
                    Just <| OneJumpSequence oneJumpSequence


torc : String -> RowCol
torc =
    Board.stringToRowCol


mtorc : String -> Maybe RowCol
mtorc string =
    let
        res =
            Board.stringToRowCol string
    in
    if Board.isRowColLegal res then
        Just res

    else
        Nothing


stringToOneSlideRecord : String -> Maybe OneSlideRecord
stringToOneSlideRecord string =
    case String.split "-" string of
        [ from, to ] ->
            let
                ( to2, hulkLoc ) =
                    case String.split "=" to of
                        [ _ ] ->
                            ( mtorc to, Nothing )

                        [ to3, hrc ] ->
                            ( mtorc to3
                            , Just <| torc hrc
                            )

                        _ ->
                            ( Nothing, Nothing )
            in
            case to2 of
                Nothing ->
                    Nothing

                Just to3 ->
                    let
                        isHulkLocLegal =
                            case hulkLoc of
                                Nothing ->
                                    True

                                Just hl ->
                                    Board.isRowColLegal hl
                    in
                    if not isHulkLocLegal then
                        Nothing

                    else
                        case mtorc from of
                            Nothing ->
                                Nothing

                            Just fromrc ->
                                Just
                                    { from = fromrc
                                    , to = to3
                                    , makeHulk = hulkLoc
                                    }

        _ ->
            Nothing


type alias ConcatibleJumps =
    { jumps : List OneCorruptibleJump
    , firstFrom : RowCol
    , firstHulkPos : Maybe RowCol
    , lastTo : RowCol
    , lastHulkPos : Maybe RowCol
    , nextOver : Maybe RowCol
    }


raiseNothings : List (Maybe a) -> Maybe (List a)
raiseNothings list =
    let
        mapper l res =
            case l of
                [] ->
                    Just <| List.reverse res

                me :: tail ->
                    case me of
                        Nothing ->
                            Nothing

                        Just e ->
                            mapper tail <| e :: res
    in
    mapper list []


listOfStringListsToOneJumpSequence : List (List String) -> Maybe (List OneCorruptibleJump)
listOfStringListsToOneJumpSequence listOfStringLists =
    case
        List.map listOfStringsToConcatibleJumps listOfStringLists
            |> raiseNothings
    of
        Nothing ->
            Nothing

        Just jumps ->
            concatConcatibleJumps jumps


type alias FromSlashOver =
    { from : RowCol
    , maybeOver : Maybe RowCol
    , hulkPos : Maybe RowCol
    }


defaultOver : RowCol -> Maybe RowCol -> RowCol -> Maybe RowCol
defaultOver from maybeOver to =
    case maybeOver of
        Just o ->
            Just o

        Nothing ->
            case locBetween from to of
                Nothing ->
                    Nothing

                Just loc ->
                    Just loc


stringToFromSlashOver : String -> Maybe FromSlashOver
stringToFromSlashOver string =
    let
        fromAndHulk : String -> Maybe ( RowCol, Maybe RowCol )
        fromAndHulk s =
            case String.split "=" s of
                [ from ] ->
                    case mtorc from of
                        Nothing ->
                            Nothing

                        Just fromrc ->
                            Just ( fromrc, Nothing )

                [ from, hulk ] ->
                    case mtorc from of
                        Nothing ->
                            Nothing

                        Just fromrc ->
                            case mtorc hulk of
                                Nothing ->
                                    Nothing

                                Just hulkrc ->
                                    Just ( fromrc, Just hulkrc )

                _ ->
                    Nothing
    in
    case String.split "/" string of
        [ from ] ->
            case fromAndHulk from of
                Nothing ->
                    Nothing

                Just ( fromrc, hulkPos ) ->
                    Just
                        { from = fromrc
                        , maybeOver = Nothing
                        , hulkPos = hulkPos
                        }

        [ from, over ] ->
            case fromAndHulk from of
                Nothing ->
                    Nothing

                Just ( fromrc, hulkPos ) ->
                    case mtorc over of
                        Nothing ->
                            Nothing

                        Just overrc ->
                            Just
                                { from = fromrc
                                , maybeOver = Just overrc
                                , hulkPos = hulkPos
                                }

        _ ->
            Nothing


listOfStringsToConcatibleJumps : List String -> Maybe ConcatibleJumps
listOfStringsToConcatibleJumps stringList =
    -- TODO
    let
        mapper : FromSlashOver -> FromSlashOver -> List FromSlashOver -> List OneCorruptibleJump -> Maybe ConcatibleJumps
        mapper firstSlashOver prevSlashOver fromSlashOvers res =
            case fromSlashOvers of
                [] ->
                    Just <|
                        { jumps = List.reverse res
                        , firstFrom = firstSlashOver.from
                        , firstHulkPos = firstSlashOver.hulkPos
                        , lastTo = prevSlashOver.from
                        , lastHulkPos = prevSlashOver.hulkPos
                        , nextOver = prevSlashOver.maybeOver
                        }

                nextFromSlashOver :: tail ->
                    let
                        from =
                            prevSlashOver.from

                        toHulk =
                            nextFromSlashOver.hulkPos

                        to =
                            nextFromSlashOver.from

                        maybeOver =
                            defaultOver from prevSlashOver.maybeOver to
                    in
                    case maybeOver of
                        Nothing ->
                            Nothing

                        Just over ->
                            if toHulk /= Nothing then
                                Nothing

                            else
                                mapper firstSlashOver nextFromSlashOver tail <|
                                    { from = from
                                    , over = over
                                    , to = to
                                    , hulkAfterJump = CorruptAfterJump
                                    }
                                        :: res
    in
    case List.map stringToFromSlashOver stringList |> raiseNothings of
        Nothing ->
            Nothing

        Just slashOvers ->
            case slashOvers of
                [] ->
                    Nothing

                slashOver :: tail ->
                    mapper slashOver slashOver tail []


concatConcatibleJumps : List ConcatibleJumps -> Maybe (List OneCorruptibleJump)
concatConcatibleJumps maybeJumpss =
    let
        mapper : RowCol -> Maybe RowCol -> List ConcatibleJumps -> List OneCorruptibleJump -> Maybe (List OneCorruptibleJump)
        mapper prevTo maybeOver jumpss res =
            case jumpss of
                [] ->
                    Just <| List.reverse res

                { jumps, firstFrom, firstHulkPos, lastTo, nextOver } :: rest ->
                    let
                        maybePrevOver =
                            defaultOver prevTo maybeOver firstFrom
                    in
                    case maybePrevOver of
                        Nothing ->
                            Nothing

                        Just prevOver ->
                            if rest /= [] && firstHulkPos /= Nothing then
                                -- Only the last jump can make a hulk
                                -- (by landing on the sanctum).
                                Nothing

                            else
                                mapper lastTo nextOver rest <|
                                    List.reverse jumps
                                        ++ ({ from = prevTo
                                            , over = prevOver
                                            , to = firstFrom
                                            , hulkAfterJump =
                                                case firstHulkPos of
                                                    Nothing ->
                                                        NoHulkAfterJump

                                                    Just hulkPos ->
                                                        MakeHulkAfterJump hulkPos
                                            }
                                                :: res
                                           )
    in
    case maybeJumpss of
        [] ->
            Just []

        { jumps, lastTo, lastHulkPos, nextOver } :: rest ->
            case lastHulkPos of
                Just _ ->
                    Nothing

                Nothing ->
                    mapper lastTo nextOver rest <| List.reverse jumps


defaultOneMove : OneMove
defaultOneMove =
    { piece = Types.emptyPiece
    , isUnique = True
    , sequence = OneSlide { from = torc "a1", to = torc "a2", makeHulk = Nothing }
    , winner = NoWinner
    , time = Types.posixZero
    }


encodeOneMove : OneMove -> Value
encodeOneMove oneMove =
    oneMoveToString oneMove |> JE.string


oneMoveDecoder : Decoder OneMove
oneMoveDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case stringToOneMove s of
                    Nothing ->
                        JD.fail <| "Not a legal oneMove string: " ++ s

                    Just oneMove ->
                        JD.succeed oneMove
            )


encodeUndoState : UndoState -> Value
encodeUndoState { board, moves, selected, legalMoves } =
    JE.object
        [ ( "board", encodeBoard board )
        , ( "moves", encodeOneMoveList moves )
        , ( "selected", encodeMaybe encodeRowCol selected )
        , ( "legalMoves", encodeMovesOrJumps legalMoves )
        ]


undoStateDecoder : Decoder UndoState
undoStateDecoder =
    JD.succeed UndoState
        |> required "board" newBoardDecoder
        |> required "moves" oneMoveListDecoder
        |> required "selected" (JD.nullable rowColDecoder)
        |> required "legalMoves" movesOrJumpsDecoder


encodeOneMoveList : List OneMove -> Value
encodeOneMoveList moves =
    let
        mapper : OneMove -> ( Int, List OneMove ) -> ( Int, List OneMove )
        mapper move ( lastTime, res ) =
            let
                time =
                    move.time |> Time.posixToMillis

                adjusted =
                    -- I get negatives if I just do "// 100" here
                    (time - lastTime)
                        |> toFloat
                        |> (*) 0.01
                        |> round
            in
            ( time
            , { move | time = adjusted |> Time.millisToPosix }
                :: res
            )

        reducedMoves =
            List.foldr mapper ( 0, [] ) moves
                |> Tuple.second
    in
    JE.list encodeOneMove reducedMoves


tenYearsInTenthsOfSeconds : Int
tenYearsInTenthsOfSeconds =
    10 * 60 * 60 * 24 * 365 * 10


oneMoveListDecoder : Decoder (List OneMove)
oneMoveListDecoder =
    JD.list oneMoveDecoder
        |> JD.andThen
            (\moves ->
                case List.head moves of
                    Nothing ->
                        JD.succeed moves

                    Just firstMove ->
                        if Time.posixToMillis firstMove.time > tenYearsInTenthsOfSeconds then
                            -- Backward compatibility
                            JD.succeed moves

                        else
                            let
                                mapper : OneMove -> ( Int, List OneMove ) -> ( Int, List OneMove )
                                mapper move ( lastTime, res ) =
                                    let
                                        time =
                                            move.time |> Time.posixToMillis

                                        adjusted =
                                            time * 100 + lastTime
                                    in
                                    ( adjusted
                                    , { move | time = adjusted |> Time.millisToPosix }
                                        :: res
                                    )
                            in
                            List.foldr mapper ( 0, [] ) moves
                                |> Tuple.second
                                |> JD.succeed
            )


encodeTestModeInitialState : TestModeInitialState -> Value
encodeTestModeInitialState state =
    let
        { board, moves, whoseTurn, selected, legalMoves } =
            state
    in
    JE.object
        [ ( "board", encodeBoard board )
        , ( "moves", encodeOneMoveList moves )
        , ( "whoseTurn", encodePlayer whoseTurn )
        , ( "selected", encodeMaybe encodeRowCol selected )
        , ( "legalMoves", encodeMovesOrJumps legalMoves )
        ]


testModeInitialStateDecoder : Decoder TestModeInitialState
testModeInitialStateDecoder =
    JD.succeed TestModeInitialState
        |> required "board" newBoardDecoder
        |> required "moves" oneMoveListDecoder
        |> required "whoseTurn" playerDecoder
        |> required "selected" (JD.nullable rowColDecoder)
        |> required "legalMoves" movesOrJumpsDecoder


encodeInitialBoard : InitialBoard -> Value
encodeInitialBoard { board, whoseTurn } =
    JE.object
        [ ( "board", encodeBoard board )
        , ( "whoseTurn", encodePlayer whoseTurn )
        ]


initialBoardDecoder : Decoder InitialBoard
initialBoardDecoder =
    JD.succeed InitialBoard
        |> required "board" newBoardDecoder
        |> required "whoseTurn" playerDecoder


encodeRequestUndo : RequestUndo -> Value
encodeRequestUndo requestUndo =
    case requestUndo of
        NoRequestUndo ->
            JE.string "NoRequestUndo"

        RequestUndo message ->
            JE.object
                [ ( "requestUndo", JE.string "request" )
                , ( "message", JE.string message )
                ]

        DenyUndo message ->
            JE.object
                [ ( "requestUndo", JE.string "deny" )
                , ( "message", JE.string message )
                ]


requestUndoDecoder : Decoder RequestUndo
requestUndoDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if s == "NoRequestUndo" then
                        JD.succeed NoRequestUndo

                    else
                        JD.fail <| "Unknown RequestUndo string" ++ s
                )
        , JD.field "requestUndo" JD.string
            |> JD.andThen
                (\s ->
                    let
                        tryit tag =
                            JD.succeed tag
                                |> required "message" JD.string
                    in
                    if s == "request" then
                        tryit RequestUndo

                    else if s == "deny" then
                        tryit DenyUndo

                    else
                        JD.fail <| "Unknown RequestUndo type: " ++ s
                )
        ]


encodeGameState : Bool -> GameState -> Value
encodeGameState includePrivate gameState =
    let
        { newBoard, initialBoard, moves, players, whoseTurn, selected, jumperLocations, legalMoves, undoStates, jumps, score, winner, requestUndo, testMode, testModeInitialState } =
            gameState

        privateValue =
            if includePrivate then
                encodePrivateGameState gameState.private

            else
                JE.null
    in
    JE.object
        [ ( "newBoard", encodeBoard newBoard )
        , ( "initialBoard", encodeMaybe encodeInitialBoard initialBoard )
        , ( "moves", encodeOneMoveList moves )
        , ( "players", encodePlayerNames players )
        , ( "whoseTurn", encodePlayer whoseTurn )
        , ( "selected", encodeMaybe encodeRowCol selected )
        , ( "jumperLocations", JE.list encodeRowCol jumperLocations )
        , ( "legalMoves", encodeMovesOrJumps legalMoves )
        , ( "undoStates", JE.list encodeUndoState undoStates )
        , ( "jumps", encodeCorruptibleJumpSequence jumps )
        , ( "score", encodeScore score )
        , ( "winner", encodeWinner winner )
        , ( "requestUndo", encodeRequestUndo requestUndo )
        , ( "testMode", encodeMaybe encodeTestMode testMode )
        , ( "testModeInitialState", encodeMaybe encodeTestModeInitialState testModeInitialState )
        , ( "private", privateValue )
        ]


gameStateDecoder : Decoder GameState
gameStateDecoder =
    JD.succeed GameState
        |> required "newBoard" newBoardDecoder
        |> optional "initialBoard" (JD.nullable initialBoardDecoder) Nothing
        |> required "moves" oneMoveListDecoder
        |> required "players" playerNamesDecoder
        |> required "whoseTurn" playerDecoder
        |> required "selected" (JD.nullable rowColDecoder)
        |> optional "jumperLocations" (JD.list rowColDecoder) []
        |> required "legalMoves" movesOrJumpsDecoder
        |> required "undoStates" (JD.list undoStateDecoder)
        |> required "jumps" corruptibleJumpSequenceDecoder
        |> required "score" scoreDecoder
        |> required "winner" winnerDecoder
        |> optional "requestUndo" requestUndoDecoder NoRequestUndo
        |> required "testMode" (JD.nullable testModeDecoder)
        |> optional "testModeInitialState" (JD.nullable testModeInitialStateDecoder) Nothing
        |> required "private" privateGameStateDecoder


encodeArchivedGame : ArchivedGame -> Value
encodeArchivedGame gameState =
    let
        { moves, players, winner, initialBoard } =
            gameState
    in
    JE.object
        [ ( "moves", encodeOneMoveList moves )
        , ( "players", encodePlayerNames players )
        , ( "winner", encodeWinner winner )
        , ( "initialBoard", encodeMaybe encodeInitialBoard initialBoard )
        ]


archivedGameDecoder : Decoder ArchivedGame
archivedGameDecoder =
    JD.succeed ArchivedGame
        |> required "moves" oneMoveListDecoder
        |> required "players" playerNamesDecoder
        |> required "winner" winnerDecoder
        |> optional "initialBoard" (JD.nullable initialBoardDecoder) Nothing


encodeRowCol : RowCol -> Value
encodeRowCol rc =
    Board.rowColToString rc |> JE.string


rowColDecoder : Decoder RowCol
rowColDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    let
                        rc =
                            Board.stringToRowCol s
                    in
                    if Board.isRowColLegal rc then
                        JD.succeed rc

                    else
                        JD.fail <| "Not a valid board location: " ++ s
                )
        , JD.succeed RowCol
            |> required "row" JD.int
            |> required "col" JD.int
        ]


encodeRowColList : List RowCol -> Value
encodeRowColList rowcols =
    JE.list encodeRowCol rowcols


rowColListDecoder : Decoder (List RowCol)
rowColListDecoder =
    JD.list rowColDecoder


encodeJumpSequence : JumpSequence -> Value
encodeJumpSequence jumps =
    JE.list encodeOneJump jumps


jumpSequenceDecoder : Decoder JumpSequence
jumpSequenceDecoder =
    JD.list oneJumpDecoder


encodeCorruptibleJumpSequence : List OneCorruptibleJump -> Value
encodeCorruptibleJumpSequence jumps =
    JE.list encodeOneCorruptibleJump jumps


corruptibleJumpSequenceDecoder : Decoder (List OneCorruptibleJump)
corruptibleJumpSequenceDecoder =
    JD.list oneCorruptibleJumpDecoder


encodeOneJump : OneJump -> Value
encodeOneJump { over, to } =
    JE.object
        [ ( "over", encodeRowCol over )
        , ( "to", encodeRowCol to )
        ]


oneJumpDecoder : Decoder OneJump
oneJumpDecoder =
    JD.succeed OneJump
        |> required "over" rowColDecoder
        |> required "to" rowColDecoder


encodeOneCorruptibleJump : OneCorruptibleJump -> Value
encodeOneCorruptibleJump { from, over, to, hulkAfterJump } =
    JE.object <|
        [ ( "from", encodeRowCol from )
        , ( "over", encodeRowCol over )
        , ( "to", encodeRowCol to )
        ]
            ++ (case hulkAfterJump of
                    NoHulkAfterJump ->
                        []

                    CorruptAfterJump ->
                        [ ( "corrupted", JE.bool True ) ]

                    MakeHulkAfterJump rowCol ->
                        [ ( "makehulk", encodeRowCol rowCol ) ]
               )


oneCorruptibleJumpDecoder : Decoder OneCorruptibleJump
oneCorruptibleJumpDecoder =
    JD.succeed OneCorruptibleJump
        |> required "from" rowColDecoder
        |> required "over" rowColDecoder
        |> required "to" rowColDecoder
        |> custom
            (JD.oneOf
                [ JD.field "corrupted" JD.bool
                    |> JD.andThen
                        (\corrupted ->
                            if corrupted then
                                JD.succeed CorruptAfterJump

                            else
                                -- This only happens for backward compatibility
                                JD.succeed NoHulkAfterJump
                        )
                , JD.field "makehulk" rowColDecoder
                    |> JD.andThen (MakeHulkAfterJump >> JD.succeed)
                , JD.succeed NoHulkAfterJump
                ]
            )


encodeMovesOrJumps : MovesOrJumps -> Value
encodeMovesOrJumps movesOrJumps =
    case movesOrJumps of
        NoMoves ->
            JE.string "NoMoves"

        Moves rowcols ->
            JE.object [ ( "moves", encodeRowColList rowcols ) ]

        Jumps jumps ->
            JE.object [ ( "jumps", JE.list encodeJumpSequence jumps ) ]


movesOrJumpsDecoder : Decoder MovesOrJumps
movesOrJumpsDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if "NoMoves" == s then
                        JD.succeed NoMoves

                    else
                        JD.fail <| "Unknown MovesOrJumps: " ++ s
                )
        , JD.field "moves" rowColListDecoder
            |> JD.andThen (\rowcols -> JD.succeed <| Moves rowcols)
        , JD.field "jumps" (JD.list jumpSequenceDecoder)
            |> JD.andThen (\sequences -> JD.succeed <| Jumps sequences)
        ]


encodeUndoWhichJumps : UndoWhichJumps -> Value
encodeUndoWhichJumps undoWhichJumps =
    JE.string
        (case undoWhichJumps of
            UndoOneJump ->
                "UndoOneJump"

            UndoAllJumps ->
                "UndoAllJumps"
        )


undoWhichJumpsDecoder : Decoder UndoWhichJumps
undoWhichJumpsDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                if s == "UndoOneJump" then
                    JD.succeed UndoOneJump

                else if s == "UndoAllJumps" then
                    JD.succeed UndoAllJumps

                else
                    JD.fail <| "Unknown UndoWhichJumps: " ++ s
            )


encodeChooseMoveOption : ChooseMoveOption -> Value
encodeChooseMoveOption option =
    case option of
        NoOption ->
            JE.string "NoOption"

        CorruptJumped ->
            JE.string "CorruptJumped"

        MakeHulk rowCol ->
            JE.object [ ( "MakeHulk", encodeRowCol rowCol ) ]


chooseMoveOptionDecoder : Decoder ChooseMoveOption
chooseMoveOptionDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\string ->
                    if string == "NoOption" then
                        JD.succeed NoOption

                    else if string == "CorruptJumped" then
                        JD.succeed CorruptJumped

                    else
                        JD.fail <| "\"CorruptJumped\" /= \"" ++ string ++ "\""
                )
        , JD.field "MakeHulk" rowColDecoder
            |> JD.andThen
                (\rowCol ->
                    JD.succeed <| MakeHulk rowCol
                )
        ]


encodeChoice : Choice -> Value
encodeChoice choice =
    JE.object
        [ case choice of
            ChoosePiece rowCol ->
                ( "ChoosePiece", encodeRowCol rowCol )

            ChooseMove rowCol option ->
                ( "ChooseMove"
                , if option == NoOption then
                    encodeRowCol rowCol

                  else
                    JE.object
                        [ ( "rowCol", encodeRowCol rowCol )
                        , ( "option", encodeChooseMoveOption option )
                        ]
                )

            ChooseUndoJump undoWhichJumps ->
                ( "ChooseUndoJump", encodeUndoWhichJumps undoWhichJumps )

            ChooseRequestUndo message ->
                ( "ChooseRequestUndo", JE.string message )

            ChooseAcceptUndo ->
                ( "ChooseAcceptUndo", JE.bool True )

            ChooseDenyUndo message ->
                ( "ChooseDenyUndo", JE.string message )

            ChooseResign player ->
                ( "ChooseResign", encodePlayer player )

            ChooseNew player ->
                ( "ChooseNew", encodePlayer player )
        ]


choiceDecoder : Decoder Choice
choiceDecoder =
    JD.oneOf
        [ JD.field "ChoosePiece" rowColDecoder
            |> JD.andThen (ChoosePiece >> JD.succeed)
        , JD.field "ChooseMove" <|
            JD.oneOf
                [ rowColDecoder
                    |> JD.andThen
                        (\rowCol ->
                            ChooseMove rowCol NoOption |> JD.succeed
                        )
                , JD.succeed ChooseMove
                    |> required "rowCol" rowColDecoder
                    |> optional "option" chooseMoveOptionDecoder NoOption
                ]
        , JD.field "ChooseUndoJump" undoWhichJumpsDecoder
            |> JD.andThen (ChooseUndoJump >> JD.succeed)
        , JD.field "ChooseRequestUndo" JD.string
            |> JD.andThen (ChooseRequestUndo >> JD.succeed)
        , JD.field "ChooseAcceptUndo" JD.bool
            |> JD.andThen (\_ -> JD.succeed ChooseAcceptUndo)
        , JD.field "ChooseDenyUndo" JD.string
            |> JD.andThen (ChooseDenyUndo >> JD.succeed)
        , JD.field "ChooseResign" playerDecoder
            |> JD.andThen (ChooseResign >> JD.succeed)
        , JD.field "ChooseNew" playerDecoder
            |> JD.andThen (ChooseNew >> JD.succeed)
        ]


encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder maybe =
    case maybe of
        Nothing ->
            JE.null

        Just a ->
            encoder a


encodePublicGame : PublicGame -> Value
encodePublicGame game =
    let
        { gameid, creator, player, forName } =
            game
    in
    JE.object
        [ ( "gameid", JE.string gameid )
        , ( "creator", JE.string creator )
        , ( "player", encodePlayer player )
        , ( "forName", encodeMaybe JE.string forName )
        ]


publicGameDecoder : Decoder PublicGame
publicGameDecoder =
    JD.succeed PublicGame
        |> required "gameid" JD.string
        |> required "creator" JD.string
        |> required "player" playerDecoder
        |> required "forName" (JD.nullable JD.string)


encodePublicGameAndPlayers : PublicGameAndPlayers -> Value
encodePublicGameAndPlayers { publicGame, players, watchers, moves, startTime, endTime } =
    JE.object
        [ ( "publicGame", encodePublicGame publicGame )
        , ( "players", encodePlayerNames players )
        , ( "watchers", JE.int watchers )
        , ( "moves", JE.int moves )
        , ( "startTime", JE.int <| Time.posixToMillis startTime )
        , ( "endTime", JE.int <| Time.posixToMillis endTime )
        ]


publicGameAndPlayersDecoder : Decoder PublicGameAndPlayers
publicGameAndPlayersDecoder =
    JD.succeed PublicGameAndPlayers
        |> required "publicGame" publicGameDecoder
        |> required "players" playerNamesDecoder
        |> required "watchers" JD.int
        |> optional "moves" JD.int 0
        |> optional "startTime" (JD.int |> JD.andThen (Time.millisToPosix >> JD.succeed)) Types.posixZero
        |> optional "endTime" (JD.int |> JD.andThen (Time.millisToPosix >> JD.succeed)) Types.posixZero


publicGameToFramework : PublicGame -> WebSocketFramework.Types.PublicGame
publicGameToFramework publicGame =
    { gameid = publicGame.gameid
    , playerName =
        encodePublicGame publicGame
            |> JE.encode 0
    }


frameworkToPublicGame : WebSocketFramework.Types.PublicGame -> Maybe PublicGame
frameworkToPublicGame { gameid, playerName } =
    JD.decodeString publicGameDecoder
        playerName
        |> Result.toMaybe


encodePublicType : PublicType -> Value
encodePublicType publicType =
    case publicType of
        NotPublic ->
            JE.string "NotPublic"

        EntirelyPublic ->
            JE.string "EntirelyPublic"

        PublicFor name ->
            JE.object [ ( "publicFor", JE.string name ) ]


publicTypeDecoder : Decoder PublicType
publicTypeDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    case s of
                        "NotPublic" ->
                            JD.succeed NotPublic

                        "EntirelyPublic" ->
                            JD.succeed EntirelyPublic

                        _ ->
                            JD.fail "Not a public type"
                )
        , JD.field "publicFor" JD.string
            |> JD.andThen (PublicFor >> JD.succeed)
        ]


encodeMessageForLog : Message -> ( ReqRsp, List ( String, String ) )
encodeMessageForLog message =
    let
        ( reqRsp, plist ) =
            messageEncoder message
    in
    ( reqRsp, List.map (\( k, v ) -> ( k, JE.encode 0 v )) plist )


messageEncoder : Message -> ( ReqRsp, Plist )
messageEncoder =
    messageEncoderInternal False


messageEncoderWithPrivate : Message -> ( ReqRsp, Plist )
messageEncoderWithPrivate =
    messageEncoderInternal True


messageEncoderInternal : Bool -> Message -> ( ReqRsp, Plist )
messageEncoderInternal includePrivate message =
    case message of
        NewReq { name, player, publicType, gamename, restoreState, maybeGameid } ->
            ( Req "new"
            , [ ( "name", JE.string name )
              , ( "player", encodePlayer player )
              , ( "publicType", encodePublicType publicType )
              , ( "gamename", JE.string gamename )
              , ( "restoreState"
                , encodeMaybe (encodeGameState includePrivate) restoreState
                )
              , ( "maybeGameid", encodeMaybe JE.string maybeGameid )
              ]
            )

        NewRsp { gameid, playerid, player, name, publicType, gamename, gameState, wasRestored } ->
            ( Rsp "new"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              , ( "player", encodePlayer player )
              , ( "name", JE.string name )
              , ( "publicType", encodePublicType publicType )
              , ( "gamename", JE.string gamename )
              , ( "gameState", encodeGameState includePrivate gameState )
              , ( "wasRestored", JE.bool wasRestored )
              ]
            )

        JoinReq { gameid, name, isRestore, inCrowd } ->
            ( Req "join"
            , [ ( "gameid", JE.string gameid )
              , ( "name", JE.string name )
              , ( "isRestore", JE.bool isRestore )
              , ( "inCrowd", JE.bool inCrowd )
              ]
            )

        ReJoinReq { gameid, playerid } ->
            ( Req "rejoin"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              ]
            )

        JoinRsp { gameid, playerid, participant, gameState, wasRestored } ->
            ( Rsp "join"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", encodeMaybe JE.string playerid )
              , ( "participant", encodeParticipant participant )
              , ( "gameState", encodeGameState includePrivate gameState )
              , ( "wasRestored", JE.bool wasRestored )
              ]
            )

        LeaveReq { playerid } ->
            ( Req "leave"
            , [ ( "playerid", JE.string playerid ) ]
            )

        LeaveRsp { gameid, participant } ->
            ( Rsp "leave"
            , [ ( "gameid", JE.string gameid )
              , ( "participant", encodeParticipant participant )
              ]
            )

        SetGameStateReq { playerid, gameState } ->
            ( Req "setGameState"
            , [ ( "playerid", JE.string playerid )
              , ( "gameState", encodeGameState includePrivate gameState )
              ]
            )

        UpdateReq { playerid } ->
            ( Req "update"
            , [ ( "playerid", JE.string playerid ) ]
            )

        UpdateRsp { gameid, gameState } ->
            ( Rsp "update"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              ]
            )

        PlayReq { playerid, placement } ->
            ( Req "play"
            , [ ( "playerid", JE.string playerid )
              , ( "placement", encodeChoice placement )
              ]
            )

        PlayRsp { gameid, gameState } ->
            ( Rsp "play"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              ]
            )

        ResignRsp { gameid, gameState, player } ->
            ( Rsp "resign"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              , ( "player", encodePlayer player )
              ]
            )

        AnotherGameRsp { gameid, gameState, player } ->
            ( Rsp "anotherGame"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              , ( "player", encodePlayer player )
              ]
            )

        GameOverRsp { gameid, gameState } ->
            ( Rsp "gameOver"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
              ]
            )

        PublicGamesReq { subscribe, forName, gameid } ->
            ( Req "publicGames"
            , [ ( "subscribe", JE.bool subscribe )
              , ( "forName", JE.string forName )
              , ( "gameid", encodeMaybe JE.string gameid )
              ]
            )

        PublicGamesRsp { games } ->
            ( Rsp "publicGames"
            , [ ( "games", JE.list encodePublicGameAndPlayers games )
              ]
            )

        PublicGamesUpdateRsp { added, removed } ->
            ( Rsp "publicGamesUpdate"
            , List.concat
                [ case added of
                    [] ->
                        []

                    games ->
                        [ ( "added", JE.list encodePublicGameAndPlayers games ) ]
                , case removed of
                    [] ->
                        []

                    gameids ->
                        [ ( "removed", JE.list JE.string gameids ) ]
                ]
            )

        StatisticsReq { subscribe } ->
            ( Req "statistics"
            , [ ( "subscribe", JE.bool subscribe ) ]
            )

        StatisticsRsp { statistics, startTime, updateTime } ->
            ( Rsp "statistics"
            , [ ( "statistics", encodeMaybe WSFED.encodeStatistics statistics )
              , ( "startTime", encodeMaybe JE.int startTime )
              , ( "updateTime", encodeMaybe JE.int updateTime )
              ]
            )

        ErrorRsp { request, text } ->
            ( Rsp "error"
            , [ ( "request", JE.string request )
              , ( "text", JE.string text )
              ]
            )

        ChatReq { playerid, text } ->
            ( Req "chat"
            , [ ( "playerid", JE.string playerid )
              , ( "text", JE.string text )
              ]
            )

        ChatRsp { gameid, name, text } ->
            ( Rsp "chat"
            , [ ( "gameid", JE.string gameid )
              , ( "name", JE.string name )
              , ( "text", JE.string text )
              ]
            )


newReqDecoder : Decoder Message
newReqDecoder =
    JD.succeed
        (\name player publicType gamename restoreState maybeGameid ->
            NewReq
                { name = name
                , player = player
                , publicType = publicType
                , gamename = gamename
                , restoreState = restoreState
                , maybeGameid = maybeGameid
                }
        )
        |> required "name" JD.string
        |> required "player" playerDecoder
        |> required "publicType" publicTypeDecoder
        |> optional "gamename" JD.string ""
        |> required "restoreState" (JD.nullable gameStateDecoder)
        |> optional "maybeGameid" (JD.nullable JD.string) Nothing


joinReqDecoder : Decoder Message
joinReqDecoder =
    JD.succeed
        (\gameid name isRestore inCrowd ->
            JoinReq
                { gameid = gameid
                , name = name
                , isRestore = isRestore
                , inCrowd = inCrowd
                }
        )
        |> required "gameid" JD.string
        |> required "name" JD.string
        |> optional "isRestore" JD.bool False
        |> optional "inCrowd" JD.bool False


rejoinReqDecoder : Decoder Message
rejoinReqDecoder =
    JD.succeed
        (\gameid playerid ->
            ReJoinReq
                { gameid = gameid
                , playerid = playerid
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" JD.string


leaveReqDecoder : Decoder Message
leaveReqDecoder =
    JD.succeed
        (\playerid ->
            LeaveReq
                { playerid = playerid
                }
        )
        |> required "playerid" JD.string


setGameStateReqDecoder : Decoder Message
setGameStateReqDecoder =
    JD.succeed
        (\playerid gameState ->
            SetGameStateReq
                { playerid = playerid
                , gameState = gameState
                }
        )
        |> required "playerid" JD.string
        |> required "gameState" gameStateDecoder


updateReqDecoder : Decoder Message
updateReqDecoder =
    JD.succeed
        (\playerid ->
            UpdateReq
                { playerid = playerid
                }
        )
        |> required "playerid" JD.string


playReqDecoder : Decoder Message
playReqDecoder =
    JD.succeed
        (\playerid placement ->
            PlayReq
                { playerid = playerid
                , placement = placement
                }
        )
        |> required "playerid" JD.string
        |> required "placement" choiceDecoder


chatReqDecoder : Decoder Message
chatReqDecoder =
    JD.succeed
        (\playerid text ->
            ChatReq
                { playerid = playerid
                , text = text
                }
        )
        |> required "playerid" JD.string
        |> required "text" JD.string


newRspDecoder : Decoder Message
newRspDecoder =
    JD.succeed
        (\gameid playerid player name publicType gamename gameState wasRestored ->
            NewRsp
                { gameid = gameid
                , playerid = playerid
                , player = player
                , name = name
                , publicType = publicType
                , gamename = gamename
                , gameState = gameState
                , wasRestored = wasRestored
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" JD.string
        |> required "player" playerDecoder
        |> required "name" JD.string
        |> required "publicType" publicTypeDecoder
        |> optional "gamename" JD.string ""
        |> required "gameState" gameStateDecoder
        |> optional "wasRestored" JD.bool False


joinRspDecoder : Decoder Message
joinRspDecoder =
    JD.succeed
        (\gameid playerid participant gameState wasRestored ->
            JoinRsp
                { gameid = gameid
                , playerid = playerid
                , participant = participant
                , gameState = gameState
                , wasRestored = wasRestored
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" (JD.nullable JD.string)
        |> required "participant" participantDecoder
        |> required "gameState" gameStateDecoder
        |> optional "wasRestored" JD.bool False


leaveRspDecoder : Decoder Message
leaveRspDecoder =
    JD.oneOf
        [ JD.succeed
            (\gameid participant ->
                LeaveRsp
                    { gameid = gameid
                    , participant = participant
                    }
            )
            |> required "gameid" JD.string
            |> required "participant" participantDecoder

        -- Backward compatibility
        , JD.succeed
            (\gameid player ->
                LeaveRsp
                    { gameid = gameid
                    , participant = PlayingParticipant player
                    }
            )
            |> required "gameid" JD.string
            |> required "player" playerDecoder
        ]


updateRspDecoder : Decoder Message
updateRspDecoder =
    JD.succeed
        (\gameid gameState ->
            UpdateRsp
                { gameid = gameid
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder


playRspDecoder : Decoder Message
playRspDecoder =
    JD.succeed
        (\gameid gameState ->
            PlayRsp
                { gameid = gameid
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder


resignRspDecoder : Decoder Message
resignRspDecoder =
    JD.succeed
        (\gameid gameState player ->
            ResignRsp
                { gameid = gameid
                , gameState = gameState
                , player = player
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder
        |> required "player" playerDecoder


anotherGameRspDecoder : Decoder Message
anotherGameRspDecoder =
    JD.succeed
        (\gameid gameState player ->
            AnotherGameRsp
                { gameid = gameid
                , gameState = gameState
                , player = player
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder
        |> required "player" playerDecoder


gameOverRspDecoder : Decoder Message
gameOverRspDecoder =
    JD.succeed
        (\gameid gameState ->
            GameOverRsp
                { gameid = gameid
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder


publicGamesReqDecoder : Decoder Message
publicGamesReqDecoder =
    JD.succeed
        (\subscribe forName gameid ->
            PublicGamesReq
                { subscribe = subscribe
                , forName = forName
                , gameid = gameid
                }
        )
        |> required "subscribe" JD.bool
        |> required "forName" JD.string
        |> required "gameid" (JD.nullable JD.string)


publicGamesRspDecoder : Decoder Message
publicGamesRspDecoder =
    JD.oneOf
        [ JD.succeed
            (\games ->
                PublicGamesRsp { games = games }
            )
            |> required "games" (JD.list publicGameAndPlayersDecoder)

        -- backward compatability
        , JD.succeed
            (\games ->
                let
                    pgToPgap : PublicGame -> PublicGameAndPlayers
                    pgToPgap publicGame =
                        { publicGame = publicGame
                        , players =
                            { white = "White"
                            , black = ""
                            }
                        , watchers = 0
                        , moves = 0
                        , startTime = Types.posixZero
                        , endTime = Types.posixZero
                        }
                in
                PublicGamesRsp
                    { games =
                        List.map pgToPgap games
                    }
            )
            |> required "games" (JD.list publicGameDecoder)
        ]


publicGamesUpdateRspDecoder : Decoder Message
publicGamesUpdateRspDecoder =
    JD.succeed
        (\added removed ->
            PublicGamesUpdateRsp
                { added = added
                , removed = removed
                }
        )
        |> optional "added" (JD.list publicGameAndPlayersDecoder) []
        |> optional "removed" (JD.list JD.string) []


statisticsReqDecoder : Decoder Message
statisticsReqDecoder =
    JD.succeed
        (\subscribe ->
            StatisticsReq { subscribe = subscribe }
        )
        |> optional "subscribe" JD.bool False


statisticsRspDecoder : Decoder Message
statisticsRspDecoder =
    JD.succeed
        (\statistics startTime updateTime ->
            StatisticsRsp
                { statistics = statistics
                , startTime = startTime
                , updateTime = updateTime
                }
        )
        |> optional "statistics" (JD.nullable WSFED.statisticsDecoder) Nothing
        |> optional "startTime" (JD.nullable JD.int) Nothing
        |> optional "updateTime" (JD.nullable JD.int) Nothing


errorRspDecoder : Decoder Message
errorRspDecoder =
    JD.succeed
        (\request text ->
            ErrorRsp
                { request = request
                , text = text
                }
        )
        |> required "request" JD.string
        |> required "text" JD.string


chatRspDecoder : Decoder Message
chatRspDecoder =
    JD.succeed
        (\gameid name text ->
            ChatRsp
                { gameid = gameid
                , name = name
                , text = text
                }
        )
        |> required "gameid" JD.string
        |> required "name" JD.string
        |> required "text" JD.string


messageDecoder : ( ReqRsp, Plist ) -> Result String Message
messageDecoder ( reqrsp, plist ) =
    case reqrsp of
        Req msg ->
            case msg of
                "new" ->
                    decodePlist newReqDecoder plist

                "join" ->
                    decodePlist joinReqDecoder plist

                "rejoin" ->
                    decodePlist rejoinReqDecoder plist

                "leave" ->
                    decodePlist leaveReqDecoder plist

                "setGameState" ->
                    decodePlist setGameStateReqDecoder plist

                "update" ->
                    decodePlist updateReqDecoder plist

                "play" ->
                    decodePlist playReqDecoder plist

                "publicGames" ->
                    decodePlist publicGamesReqDecoder plist

                "statistics" ->
                    decodePlist statisticsReqDecoder plist

                "chat" ->
                    decodePlist chatReqDecoder plist

                _ ->
                    Err <| "Unknown Req: " ++ msg

        Rsp msg ->
            case msg of
                "new" ->
                    decodePlist newRspDecoder plist

                "join" ->
                    decodePlist joinRspDecoder plist

                "leave" ->
                    decodePlist leaveRspDecoder plist

                "update" ->
                    decodePlist updateRspDecoder plist

                "play" ->
                    decodePlist playRspDecoder plist

                "resign" ->
                    decodePlist resignRspDecoder plist

                "anotherGame" ->
                    decodePlist anotherGameRspDecoder plist

                "gameOver" ->
                    decodePlist gameOverRspDecoder plist

                "publicGames" ->
                    decodePlist publicGamesRspDecoder plist

                "publicGamesUpdate" ->
                    decodePlist publicGamesUpdateRspDecoder plist

                "statistics" ->
                    decodePlist statisticsRspDecoder plist

                "error" ->
                    decodePlist errorRspDecoder plist

                "chat" ->
                    decodePlist chatRspDecoder plist

                _ ->
                    Err <| "Unknown Rsp: " ++ msg


encodeNamedGame : NamedGame msg -> Value
encodeNamedGame game =
    JE.object
        [ ( "gamename", JE.string game.gamename )
        , ( "gameid", JE.string game.gameid )
        , ( "gameState", encodeGameState True game.gameState )
        , ( "isLocal", JE.bool game.isLocal )
        , ( "serverUrl", JE.string game.serverUrl )
        , ( "otherPlayerid", JE.string game.otherPlayerid )
        , ( "player", encodePlayer game.player )
        , ( "watcherName", encodeMaybe JE.string game.watcherName )
        , ( "playerid", JE.string game.playerid )
        , ( "isLive", JE.bool game.isLive )
        , ( "yourWins", JE.int game.yourWins )
        , ( "archives", JE.list encodeArchivedGame game.archives )
        ]


namedGameDecoder : ServerInterface msg -> Decoder (NamedGame msg)
namedGameDecoder proxyServer =
    JD.succeed NamedGame
        |> required "gamename" JD.string
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder
        |> required "isLocal" JD.bool
        |> required "serverUrl" JD.string
        |> required "otherPlayerid" JD.string
        |> required "player" playerDecoder
        |> optional "watcherName" (JD.nullable JD.string) Nothing
        |> required "playerid" JD.string
        |> required "isLive" JD.bool
        |> required "yourWins" JD.int
        |> optional "archives" (JD.list archivedGameDecoder) []
        -- interfaceIsProxy
        |> hardcoded True
        |> hardcoded proxyServer


messageToLogMessage : Message -> MessageForLog
messageToLogMessage message =
    let
        gameStateString gameState =
            JE.encode 0 (encodeGameState True gameState)
    in
    case message of
        NewReq { name, player, publicType, gamename, restoreState, maybeGameid } ->
            NewReqLog
                { name = name
                , player = player
                , publicType = publicType
                , gamename = gamename
                , restoreState =
                    case restoreState of
                        Nothing ->
                            Nothing

                        Just gameState ->
                            Just <| gameStateString gameState
                , maybeGameid = maybeGameid
                }

        NewRsp { gameid, playerid, player, name, publicType, gamename, gameState, wasRestored } ->
            NewRspLog
                { gameid = gameid
                , playerid = playerid
                , player = player
                , name = name
                , publicType = publicType
                , gamename = gamename
                , gameState = gameStateString gameState
                , wasRestored = wasRestored
                }

        JoinReq rec ->
            JoinReqLog rec

        ReJoinReq rec ->
            RejoinReqLog rec

        JoinRsp { gameid, playerid, participant, gameState, wasRestored } ->
            JoinRspLog
                { gameid = gameid
                , playerid = playerid
                , participant = participant
                , gameState = gameStateString gameState
                , wasRestored = wasRestored
                }

        LeaveReq rec ->
            LeaveReqLog rec

        LeaveRsp rec ->
            LeaveRspLog rec

        SetGameStateReq { playerid, gameState } ->
            SetGameStateReqLog
                { playerid = playerid
                , gameState = gameStateString gameState
                }

        UpdateReq rec ->
            UpdateReqLog rec

        UpdateRsp { gameid, gameState } ->
            UpdateRspLog
                { gameid = gameid
                , gameState = gameStateString gameState
                }

        PlayReq rec ->
            PlayReqLog rec

        PlayRsp { gameid, gameState } ->
            PlayRspLog
                { gameid = gameid
                , gameState = gameStateString gameState
                }

        ResignRsp { gameid, gameState, player } ->
            ResignRspLog
                { gameid = gameid
                , gameState = gameStateString gameState
                , player = player
                }

        AnotherGameRsp { gameid, gameState, player } ->
            AnotherGameRspLog
                { gameid = gameid
                , gameState = gameStateString gameState
                , player = player
                }

        GameOverRsp { gameid, gameState } ->
            GameOverRspLog
                { gameid = gameid
                , gameState = gameStateString gameState
                }

        PublicGamesReq rec ->
            PublicGamesReqLog rec

        PublicGamesRsp rec ->
            PublicGamesRspLog rec

        PublicGamesUpdateRsp rec ->
            PublicGamesUpdateRspLog rec

        StatisticsReq rec ->
            StatisticsReqLog rec

        StatisticsRsp rec ->
            StatisticsRspLog rec

        ErrorRsp rec ->
            ErrorRspLog rec

        ChatReq rec ->
            ChatReqLog rec

        ChatRsp rec ->
            ChatRspLog rec


encodeWithTimestamp : ( Int, Value ) -> Value
encodeWithTimestamp ( timestamp, value ) =
    [ JE.int timestamp, value ]
        |> JE.list identity


withTimestampDecoder : Decoder ( Int, Value )
withTimestampDecoder =
    JD.list JD.value
        |> JD.andThen
            (\list ->
                case list of
                    [ ts, v ] ->
                        case JD.decodeValue JD.int ts of
                            Ok timestamp ->
                                JD.succeed ( timestamp, v )

                            Err _ ->
                                JD.fail <| "Not an integer: " ++ JE.encode 0 ts

                    _ ->
                        JD.fail "Not a two-element array"
            )
