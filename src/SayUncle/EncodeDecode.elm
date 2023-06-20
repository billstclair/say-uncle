---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- Say Uncle JSON encoders and decoders
-- Copyright (c) 2019-2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module SayUncle.EncodeDecode exposing
    ( boardToString
    , cardToString
    , cardsToString
    , decodeSavedModel
    , encodeGameState
    , encodeMessageForLog
    , encodeNamedGame
    , encodePublicGameAndPlayers
    , encodeSavedModel
    , encodeWithTimestamp
    , frameworkToPublicGame
    , gameStateDecoder
    , messageDecoder
    , messageEncoder
    , messageEncoderWithPrivate
    , messageToLogMessage
    , namedGameDecoder
    , publicGameAndPlayersDecoder
    , publicGameToFramework
    , stringToBoard
    , stringToCard
    , stringToCards
    , stringToMaybeCards
    , withTimestampDecoder
    )

import Array exposing (Array)
import Cards exposing (Card(..), Face(..), Suit(..))
import Deck exposing (ShuffledDeck)
import Dict exposing (Dict)
import ElmChat
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Random exposing (Seed)
import SayUncle.Board as Board
import SayUncle.Types as Types
    exposing
        ( Board
        , BoardClick(..)
        , ChatSettings
        , Choice(..)
        , GameState
        , InitialBoard
        , Message(..)
        , MessageForLog(..)
        , NamedGame
        , Page(..)
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
        , Settings
        , Socket
        , State(..)
        , StyleType(..)
        , Winner(..)
        )
import SayUncle.WhichServer as WhichServer
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


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "page", encodePage model.page )
        , ( "chooseFirst", encodePlayer model.chooseFirst )
        , ( "gameid", JE.string model.gameid )
        , ( "settings", encodeSettings model.settings )
        , ( "styleType", encodeStyleType model.styleType )
        , ( "notificationsEnabled", JE.bool model.notificationsEnabled )
        , ( "soundEnabled", JE.bool model.soundEnabled )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "page" pageDecoder MainPage
        |> required "chooseFirst" playerDecoder
        |> optional "gameid" JD.string ""
        |> optional "settings" settingsDecoder Types.emptySettings
        |> optional "styleType" styleTypeDecoder LightStyle
        |> optional "notificationsEnabled" JD.bool False
        |> optional "soundEnabled" JD.bool False


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
    JE.string <| String.fromInt player


playerDecoder : Decoder Player
playerDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case String.toInt s of
                    Just player ->
                        JD.succeed player

                    Nothing ->
                        JD.fail <| "Unknown player: " ++ s
            )


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


stringToCardDict : Dict String Card
stringToCardDict =
    List.map (\card -> ( cardToString card, card ))
        (Back :: Deck.getCards Deck.fullDeck)
        |> Dict.fromList


suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs ->
            "c"

        Diamonds ->
            "d"

        Hearts ->
            "h"

        Spades ->
            "s"


faceToString : Face -> String
faceToString face =
    case face of
        Ace ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "t"

        Jack ->
            "j"

        Queen ->
            "q"

        King ->
            "k"


cardToString : Card -> String
cardToString card =
    case card of
        Back ->
            "bb"

        Card suit face ->
            faceToString face ++ suitToString suit


stringToCard : String -> Maybe Card
stringToCard string =
    Dict.get string stringToCardDict


encodeCard : Card -> Value
encodeCard card =
    cardToString card |> JE.string


cardDecoder : Decoder Card
cardDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case stringToCard s of
                    Nothing ->
                        JD.fail <| "Not a card: " ++ s

                    Just card ->
                        JD.succeed card
            )


maybeCardToString : Maybe Card -> String
maybeCardToString maybeCard =
    case maybeCard of
        Nothing ->
            "  "

        Just card ->
            cardToString card


stringToMaybeCards : String -> List (Maybe Card)
stringToMaybeCards string =
    let
        loop : List Char -> List (Maybe Card) -> List (Maybe Card)
        loop chars res =
            case chars of
                [] ->
                    List.reverse res

                c1 :: c2 :: rest ->
                    let
                        s =
                            String.fromChar c1 ++ String.fromChar c2
                    in
                    case stringToCard s of
                        Nothing ->
                            loop rest <| Nothing :: res

                        Just card ->
                            loop rest <| Just card :: res

                _ ->
                    loop [] <| Nothing :: res
    in
    loop (String.toList string) []


stringToCards : String -> Maybe (List Card)
stringToCards string =
    let
        maybeCards =
            stringToMaybeCards string
    in
    if List.member Nothing maybeCards then
        Nothing

    else
        List.filterMap identity maybeCards
            |> Just


cardsToString : List Card -> String
cardsToString cards =
    List.map cardToString cards
        |> List.foldr (++) ""


maybeCardsToString : List (Maybe Card) -> String
maybeCardsToString cards =
    List.map maybeCardToString cards
        |> List.foldr (++) ""


boardToString : Board -> String
boardToString { tableau, stock, turnedStock, hands } =
    String.concat
        [ maybeCardsToString (Array.toList tableau)
        , "|"
        , cardsToString <| Deck.getCards stock
        , "|"
        , maybeCardToString turnedStock
        , "|"
        , Array.toList hands
            |> List.map cardsToString
            |> String.join "+"
        ]


stringToBoard : String -> Maybe Board
stringToBoard string =
    case String.split "|" string of
        [ tableauString, stockString, turnedStockString, handsString ] ->
            let
                hands =
                    String.split "+" handsString
                        |> List.map stringToCards
            in
            if List.member Nothing hands then
                Nothing

            else
                case stringToCards stockString of
                    Nothing ->
                        Nothing

                    Just stockCards ->
                        Just
                            { tableau = stringToMaybeCards tableauString |> Array.fromList
                            , stock = Deck.newDeck stockCards
                            , turnedStock = stringToCard turnedStockString
                            , hands = List.filterMap identity hands |> Array.fromList
                            , seed = zeroSeed
                            }

        _ ->
            Nothing


zeroSeed : Seed
zeroSeed =
    Random.initialSeed 0


encodeBoard : Board -> Value
encodeBoard board =
    JE.string <| boardToString board


boardDecoder : Decoder Board
boardDecoder =
    JD.string
        |> JD.andThen
            (\string ->
                case stringToBoard string of
                    Nothing ->
                        JD.fail "Invalid board string."

                    Just board ->
                        JD.succeed board
            )


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
encodePlayerNames playerNames =
    JE.dict String.fromInt JE.string playerNames


playerNamesDecoder : Decoder PlayerNames
playerNamesDecoder =
    JD.keyValuePairs JD.string
        |> JD.andThen
            (\kvs ->
                let
                    ivs =
                        List.map (\( k, v ) -> ( String.toInt k, v )) kvs
                in
                case LE.find (\( k, _ ) -> k == Nothing) ivs of
                    Just _ ->
                        JD.fail "Non-integer player name"

                    Nothing ->
                        List.filterMap
                            (\( k, v ) ->
                                case k of
                                    Nothing ->
                                        Nothing

                                    Just i ->
                                        Just ( i, v )
                            )
                            ivs
                            |> Dict.fromList
                            |> JD.succeed
            )



-- TODO


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


tenYearsInTenthsOfSeconds : Int
tenYearsInTenthsOfSeconds =
    10 * 60 * 60 * 24 * 365 * 10


encodeWinner : Winner -> Value
encodeWinner winner =
    case winner of
        NoWinner ->
            JE.string "NoWinner"

        SayUncleWinner { saidUncle, won } ->
            JE.object
                [ ( "SayUncleWinner"
                  , JE.object
                        [ ( "saidUncle", encodePlayer saidUncle )
                        , ( "won", encodePlayer won )
                        ]
                  )
                ]

        StockUsedWinner player ->
            JE.object
                [ ( "StockUsedWinner", encodePlayer player ) ]


winnerDecoder : Decoder Winner
winnerDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if s == "NoWinner" then
                        JD.succeed NoWinner

                    else
                        JD.fail <| "Unknown winner: \"" ++ s ++ "\""
                )
        , JD.field "SayUncleWinner"
            (JD.succeed
                (\saidUncle won ->
                    { saidUncle = saidUncle
                    , won = won
                    }
                )
                |> required "saidUncle" playerDecoder
                |> required "won" playerDecoder
            )
            |> JD.andThen
                (\rec ->
                    JD.succeed <| SayUncleWinner rec
                )
        , JD.field "StockUsedWinner" playerDecoder
            |> JD.andThen
                (\player ->
                    JD.succeed <| StockUsedWinner player
                )
        ]


encodeScore : Score -> Value
encodeScore { games, points } =
    JE.object
        [ ( "games", JE.int games )
        , ( "points"
          , JE.dict String.fromInt JE.int points
          )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    JD.succeed Score
        |> required "games" JD.int
        |> required "points" pointsDecoder


pointsDecoder : Decoder (Dict Int Int)
pointsDecoder =
    JD.keyValuePairs JD.int
        |> JD.andThen
            (\kvs ->
                let
                    loop : List ( String, Int ) -> List ( Int, Int ) -> Result String (Dict Int Int)
                    loop kvsTail res =
                        case kvsTail of
                            [] ->
                                Ok <| Dict.fromList res

                            ( s, i ) :: rest ->
                                case String.toInt s of
                                    Nothing ->
                                        Err <| "Not an integer: \"" ++ s ++ "\""

                                    Just si ->
                                        loop rest <| ( si, i ) :: res
                in
                case loop kvs [] of
                    Err s ->
                        JD.fail s

                    Ok dict ->
                        JD.succeed dict
            )


encodeState : State -> Value
encodeState state =
    case state of
        InitialState ->
            JE.string "InitialState"

        TableauState ->
            JE.string "TableauState"

        TurnStockState ->
            JE.string "TurnStockState"

        ChooseStockState ->
            JE.string "ChooseStockState"

        DiscardState ->
            JE.string "DiscardState"

        GameOverState winner ->
            JE.object
                [ ( "GameOverState", JE.int winner ) ]


stateDecoder : Decoder State
stateDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    case s of
                        "InitialState" ->
                            JD.succeed InitialState

                        "TableauState" ->
                            JD.succeed TableauState

                        "TurnStockState" ->
                            JD.succeed TurnStockState

                        "ChooseStockState" ->
                            JD.succeed ChooseStockState

                        "DiscardState" ->
                            JD.succeed DiscardState

                        _ ->
                            JD.fail "Illegal string state"
                )
        , JD.field "GameOverState" JD.int
            |> JD.map GameOverState
        ]


encodeGameState : Bool -> GameState -> Value
encodeGameState includePrivate gameState =
    let
        { board, maxPlayers, winningPoints, players, whoseTurn, player, state, score, winner } =
            gameState

        privateValue =
            if includePrivate then
                encodePrivateGameState gameState.private

            else
                JE.null
    in
    JE.object
        [ ( "board", encodeBoard board )
        , ( "maxPlayers", JE.int maxPlayers )
        , ( "winningPoints", JE.int winningPoints )
        , ( "players", encodePlayerNames players )
        , ( "whoseTurn", encodePlayer whoseTurn )
        , ( "player", encodePlayer player )
        , ( "state", encodeState state )
        , ( "score", encodeScore score )
        , ( "winner", encodeWinner winner )
        , ( "private", privateValue )
        ]


gameStateDecoder : Decoder GameState
gameStateDecoder =
    JD.succeed GameState
        |> required "board" boardDecoder
        |> required "maxPlayers" JD.int
        |> required "winningPoints" JD.int
        |> required "players" playerNamesDecoder
        |> required "whoseTurn" playerDecoder
        |> required "player" playerDecoder
        |> required "state" stateDecoder
        |> required "score" scoreDecoder
        |> required "winner" winnerDecoder
        |> required "private" privateGameStateDecoder


encodeChoice : Choice -> Value
encodeChoice choice =
    case choice of
        ChooseNew ->
            JE.string "ChooseNew"

        ChooseStart ->
            JE.string "ChooseStart"

        ChooseTableau card ->
            JE.object [ ( "ChooseTableau", encodeCard card ) ]

        TurnStock ->
            JE.string "TurnStock"

        ChooseStock ->
            JE.string "ChooseStock"

        SkipStock ->
            JE.string "SkipStock"

        Discard card ->
            JE.object [ ( "Discard", encodeCard card ) ]

        SayUncle ->
            JE.string "SayUncle"


choiceDecoder : Decoder Choice
choiceDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    case s of
                        "ChooseNew" ->
                            JD.succeed ChooseNew

                        "ChooseStart" ->
                            JD.succeed ChooseStart

                        "TurnStock" ->
                            JD.succeed TurnStock

                        "ChooseStock" ->
                            JD.succeed ChooseStock

                        "SkipStock" ->
                            JD.succeed SkipStock

                        "SayUncle" ->
                            JD.succeed SayUncle

                        _ ->
                            JD.fail <| "Unknown Choice: " ++ s
                )
        , JD.field "ChooseTableau" cardDecoder
            |> JD.andThen (ChooseTableau >> JD.succeed)
        , JD.field "Discard" cardDecoder
            |> JD.andThen (Discard >> JD.succeed)
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
encodePublicGameAndPlayers { publicGame, players } =
    JE.object
        [ ( "publicGame", encodePublicGame publicGame )
        , ( "players", encodePlayerNames players )
        ]


publicGameAndPlayersDecoder : Decoder PublicGameAndPlayers
publicGameAndPlayersDecoder =
    JD.succeed PublicGameAndPlayers
        |> required "publicGame" publicGameDecoder
        |> required "players" playerNamesDecoder


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
        NewReq { name, publicType, maxPlayers, winningPoints, restoreState, maybeGameid } ->
            ( Req "new"
            , [ ( "name", JE.string name )
              , ( "publicType", encodePublicType publicType )
              , ( "maxPlayers", JE.int maxPlayers )
              , ( "winningPoints", JE.int winningPoints )
              , ( "restoreState"
                , encodeMaybe (encodeGameState includePrivate) restoreState
                )
              , ( "maybeGameid", encodeMaybe JE.string maybeGameid )
              ]
            )

        NewRsp { gameid, playerid, player, name, publicType, gameState, wasRestored } ->
            ( Rsp "new"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              , ( "player", encodePlayer player )
              , ( "name", JE.string name )
              , ( "publicType", encodePublicType publicType )
              , ( "gameState", encodeGameState includePrivate gameState )
              , ( "wasRestored", JE.bool wasRestored )
              ]
            )

        JoinReq { gameid, name } ->
            ( Req "join"
            , [ ( "gameid", JE.string gameid )
              , ( "name", JE.string name )
              ]
            )

        ReJoinReq { gameid, playerid } ->
            ( Req "rejoin"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              ]
            )

        JoinRsp { gameid, playerid, gameState } ->
            ( Rsp "join"
            , [ ( "gameid", JE.string gameid )
              , ( "playerid", JE.string playerid )
              , ( "gameState", encodeGameState includePrivate gameState )
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

        LeaveReq { playerid } ->
            ( Req "leave"
            , [ ( "playerid", JE.string playerid )
              ]
            )

        LeaveRsp { gameid, participant, name } ->
            ( Rsp "leave"
            , [ ( "gameid", JE.string gameid )
              , ( "participant", JE.int participant )
              , ( "name", JE.string name )
              ]
            )

        AnotherGameRsp { gameid, gameState } ->
            ( Rsp "anotherGame"
            , [ ( "gameid", JE.string gameid )
              , ( "gameState", encodeGameState includePrivate gameState )
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
        (\name publicType maxPlayers winningPoints restoreState maybeGameid ->
            NewReq
                { name = name
                , publicType = publicType
                , maxPlayers = maxPlayers
                , winningPoints = winningPoints
                , seed = zeroSeed
                , restoreState = restoreState
                , maybeGameid = maybeGameid
                }
        )
        |> required "name" JD.string
        |> required "publicType" publicTypeDecoder
        |> required "maxPlayers" JD.int
        |> required "winningPoints" JD.int
        |> required "restoreState" (JD.nullable gameStateDecoder)
        |> optional "maybeGameid" (JD.nullable JD.string) Nothing


joinReqDecoder : Decoder Message
joinReqDecoder =
    JD.succeed
        (\gameid name ->
            JoinReq
                { gameid = gameid
                , name = name
                }
        )
        |> required "gameid" JD.string
        |> required "name" JD.string


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


leaveReqDecoder : Decoder Message
leaveReqDecoder =
    JD.succeed
        (\playerid ->
            LeaveReq
                { playerid = playerid
                }
        )
        |> required "playerid" JD.string


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
        (\gameid playerid player name publicType gameState wasRestored ->
            NewRsp
                { gameid = gameid
                , playerid = playerid
                , player = player
                , name = name
                , publicType = publicType
                , gameState = gameState
                , wasRestored = wasRestored
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" JD.string
        |> required "player" playerDecoder
        |> required "name" JD.string
        |> required "publicType" publicTypeDecoder
        |> required "gameState" gameStateDecoder
        |> optional "wasRestored" JD.bool False


joinRspDecoder : Decoder Message
joinRspDecoder =
    JD.succeed
        (\gameid playerid gameState ->
            JoinRsp
                { gameid = gameid
                , playerid = playerid
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "playerid" JD.string
        |> required "gameState" gameStateDecoder


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


leaveRspDecoder : Decoder Message
leaveRspDecoder =
    JD.succeed
        (\gameid participant name ->
            LeaveRsp
                { gameid = gameid
                , participant = participant
                , name = name
                }
        )
        |> required "gameid" JD.string
        |> required "participant" JD.int
        |> required "name" JD.string


anotherGameRspDecoder : Decoder Message
anotherGameRspDecoder =
    JD.succeed
        (\gameid gameState ->
            AnotherGameRsp
                { gameid = gameid
                , gameState = gameState
                }
        )
        |> required "gameid" JD.string
        |> required "gameState" gameStateDecoder


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
    JD.succeed
        (\games ->
            PublicGamesRsp { games = games }
        )
        |> required "games" (JD.list publicGameAndPlayersDecoder)


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

                "setGameState" ->
                    decodePlist setGameStateReqDecoder plist

                "update" ->
                    decodePlist updateReqDecoder plist

                "play" ->
                    decodePlist playReqDecoder plist

                "leave" ->
                    decodePlist leaveReqDecoder plist

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

                "update" ->
                    decodePlist updateRspDecoder plist

                "play" ->
                    decodePlist playRspDecoder plist

                "leave" ->
                    decodePlist leaveRspDecoder plist

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
        [ ( "gameid", JE.string game.gameid )
        , ( "playerIds", JE.dict identity JE.int game.playerIds )
        , ( "playerWins", JE.dict identity JE.int game.playerIds )
        , ( "gameState", encodeGameState True game.gameState )
        , ( "isLocal", JE.bool game.isLocal )
        , ( "serverUrl", JE.string game.serverUrl )
        , ( "player", encodePlayer game.player )
        , ( "playerid", JE.string game.playerid )
        , ( "isLive", JE.bool game.isLive )
        ]


namedGameDecoder : ServerInterface msg -> Decoder (NamedGame msg)
namedGameDecoder proxyServer =
    JD.succeed NamedGame
        |> required "gameid" JD.string
        |> required "playerIds" (JD.dict JD.int)
        |> required "playerWins" (JD.dict JD.int)
        |> required "gameState" gameStateDecoder
        |> required "isLocal" JD.bool
        |> required "serverUrl" JD.string
        |> required "player" playerDecoder
        |> required "playerid" JD.string
        |> required "isLive" JD.bool
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
        NewReq { name, publicType, maxPlayers, winningPoints, restoreState, maybeGameid } ->
            NewReqLog
                { name = name
                , publicType = publicType
                , maxPlayers = maxPlayers
                , winningPoints = winningPoints
                , restoreState =
                    case restoreState of
                        Nothing ->
                            Nothing

                        Just gameState ->
                            Just <| gameStateString gameState
                , maybeGameid = maybeGameid
                }

        NewRsp { gameid, playerid, player, name, publicType, gameState, wasRestored } ->
            NewRspLog
                { gameid = gameid
                , playerid = playerid
                , player = player
                , name = name
                , publicType = publicType
                , gameState = gameStateString gameState
                , wasRestored = wasRestored
                }

        JoinReq rec ->
            JoinReqLog rec

        ReJoinReq rec ->
            RejoinReqLog rec

        JoinRsp { gameid, playerid, gameState } ->
            JoinRspLog
                { gameid = gameid
                , playerid = playerid
                , gameState = gameStateString gameState
                }

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

        LeaveReq rec ->
            LeaveReqLog rec

        LeaveRsp rec ->
            LeaveRspLog rec

        AnotherGameRsp { gameState } ->
            AnotherGameRspLog
                { gameState = gameStateString gameState
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
