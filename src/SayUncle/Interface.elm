--------------------------------------------------------------------
--
-- Interface.elm
-- Say Uncle server interface.
-- Runs on local machine for local play, and server for networked play.
--
-- Copyright (c) 2019-2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module SayUncle.Interface exposing
    ( bumpStatistic
    , emptyGameState
    , forNameMatches
    , getStatisticsChanged
    , messageProcessor
    , proxyMessageProcessor
    , publicGameAddPlayers
    , setStatisticsChanged
    )

import Array exposing (Array)
import Cards exposing (Card(..), Face(..), Suit(..))
import Debug
import Deck exposing (Deck, ShuffledDeck)
import Dict exposing (Dict)
import List.Extra as LE
import Random exposing (Seed)
import SayUncle.Board as Board
import SayUncle.EncodeDecode as ED
import SayUncle.Types as Types
    exposing
        ( Board
        , Choice(..)
        , GameState
        , InitialBoard
        , Message(..)
        , Participant
        , Player
        , PlayerNames
        , PublicGame
        , PublicGameAndPlayers
        , PublicType(..)
        , RowCol
        , Score
        , ServerState
        , State(..)
        , StatisticsKeys
        , WinReason(..)
        , Winner(..)
        , statisticsKeys
        )
import SayUncle.WhichServer as WhichServer
import Time exposing (Posix)
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.EncodeDecode as WSFED
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , Plist
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerState
        )


emptyGameState : PlayerNames -> Int -> Int -> Seed -> GameState
emptyGameState players maxPlayers winningPoints seed =
    { board = Board.initial (Dict.size players) seed
    , maxPlayers = maxPlayers
    , winningPoints = winningPoints
    , players = players
    , whoseTurn = 0
    , player = 0
    , state = InitialState
    , score = Types.zeroScore
    , winner = NoWinner
    , private = Types.emptyPrivateGameState
    }


errorRes : Message -> Types.ServerState -> String -> ( Types.ServerState, Maybe Message )
errorRes message state text =
    ( state
    , Just <|
        ErrorRsp
            { request = WSFED.encodeMessage ED.messageEncoder message
            , text = text
            }
    )


forNameMatches : String -> Maybe String -> Bool
forNameMatches name1 name2 =
    (name1 |> String.toLower)
        == (Maybe.withDefault "" name2 |> String.toLower)


lookupGame : Message -> PlayerId -> Types.ServerState -> Result ( Types.ServerState, Maybe Message ) ( GameId, GameState, Participant )
lookupGame message playerid state =
    let
        err text =
            Err <| errorRes message state text
    in
    case ServerInterface.getPlayer playerid state of
        Nothing ->
            err "Unknown playerid"

        Just { gameid, player } ->
            case ServerInterface.getGame gameid state of
                Nothing ->
                    err <| "Unknown gameid: " ++ gameid

                Just gameState ->
                    Ok ( gameid, gameState, player )


getStatisticsChanged : Types.ServerState -> Bool
getStatisticsChanged state =
    case state.state of
        Nothing ->
            False

        Just gameState ->
            gameState.private.statisticsChanged


setStatisticsChanged : Bool -> Types.ServerState -> Types.ServerState
setStatisticsChanged changed state =
    case state.state of
        Nothing ->
            -- can't happen
            state

        Just gameState ->
            let
                pgs =
                    gameState.private
            in
            { state
                | state =
                    Just
                        { gameState
                            | private =
                                { pgs | statisticsChanged = changed }
                        }
            }


bumpStatistic : (StatisticsKeys -> String) -> Types.ServerState -> Types.ServerState
bumpStatistic dotProperty state =
    changeStatistic dotProperty 1 state


decrementStatistic : (StatisticsKeys -> String) -> Types.ServerState -> Types.ServerState
decrementStatistic dotProperty state =
    changeStatistic dotProperty -1 state


changeStatistic : (StatisticsKeys -> String) -> Int -> Types.ServerState -> Types.ServerState
changeStatistic dotProperty delta state =
    let
        property =
            dotProperty statisticsKeys

        state2 =
            case state.statistics of
                Just _ ->
                    state

                Nothing ->
                    -- This automatically enables statistics.
                    -- May want it to be an environment variable for the server.
                    { state
                        | statistics =
                            Just <| WebSocketFramework.Types.emptyStatistics
                    }

        value =
            case ServerInterface.getStatisticsProperty property state2 of
                Nothing ->
                    0

                Just v ->
                    v

        res =
            ServerInterface.setStatisticsProperty property (Just <| value + delta) state2

        stats =
            res.statistics
    in
    res


messageProcessor : Types.ServerState -> Message -> ( Types.ServerState, Maybe Message )
messageProcessor =
    generalMessageProcessor False


proxyMessageProcessor : Types.ServerState -> Message -> ( Types.ServerState, Maybe Message )
proxyMessageProcessor =
    generalMessageProcessor True


generalMessageProcessor : Bool -> Types.ServerState -> Message -> ( Types.ServerState, Maybe Message )
generalMessageProcessor isProxyServer state message =
    let
        ( newState, response ) =
            generalMessageProcessorInternal isProxyServer state message

        newState2 =
            if isProxyServer || state.statistics == newState.statistics then
                newState

            else
                setStatisticsChanged True newState

        newState3 =
            if isProxyServer then
                -- Prevent garbage accumulation.
                -- This is only processed by the real server.
                { newState2 | changes = Nothing }

            else
                newState2
    in
    ( newState3, response )


logInterfaceSeed : String -> Types.ServerInterface msg -> Types.ServerInterface msg
logInterfaceSeed prefix (ServerInterface interface) =
    case interface.state of
        Nothing ->
            ServerInterface interface

        Just state ->
            let
                state2 =
                    logSeed prefix state
            in
            ServerInterface interface


logSeed : String -> Types.ServerState -> Types.ServerState
logSeed prefix state =
    let
        label =
            if prefix /= "" then
                prefix ++ ", seed"

            else
                "seed"

        seed =
            Debug.log label state.seed
    in
    state


setNextPlayer : GameState -> GameState
setNextPlayer gameState =
    let
        player =
            gameState.player + 1
    in
    { gameState
        | player =
            if player >= Dict.size gameState.players then
                0

            else
                player
    }


generalMessageProcessorInternal : Bool -> Types.ServerState -> Message -> ( Types.ServerState, Maybe Message )
generalMessageProcessorInternal isProxyServer state message =
    let
        time =
            state.time
    in
    case message of
        NewReq { name, publicType, maxPlayers, winningPoints, seed, restoreState, maybeGameid } ->
            let
                gameidError =
                    case maybeGameid of
                        Nothing ->
                            ""

                        Just gameid ->
                            case ServerInterface.getGame gameid state of
                                Nothing ->
                                    ""

                                Just _ ->
                                    "Can't restore existing session id"
            in
            if gameidError /= "" then
                errorRes message state gameidError

            else if name == "" then
                errorRes message state "Blank name not allowed."

            else
                let
                    players =
                        Dict.fromList [ ( 0, name ) ]

                    gameState =
                        case restoreState of
                            Nothing ->
                                emptyGameState players maxPlayers winningPoints seed

                            Just gs ->
                                { gs
                                    | players = players
                                }

                    ( gameid, state2 ) =
                        case maybeGameid of
                            Nothing ->
                                ServerInterface.newGameid state

                            Just gid ->
                                ( gid, state )

                    ( playerid, state3 ) =
                        ServerInterface.newPlayerid state2

                    playerInfo =
                        { gameid = gameid, player = 0 }

                    state4 =
                        ServerInterface.addGame gameid gameState state3
                            |> bumpStatistic .totalConnections
                            |> bumpStatistic .activeConnections

                    state5 =
                        ServerInterface.addPlayer playerid playerInfo state4

                    forName =
                        case publicType of
                            PublicFor fn ->
                                Just fn

                            _ ->
                                Nothing

                    state6 =
                        if publicType == NotPublic then
                            state5

                        else
                            let
                                publicGame =
                                    { gameid = gameid
                                    , creator = name
                                    , player = 0
                                    , forName = forName
                                    }
                                        |> ED.publicGameToFramework
                            in
                            { state5
                                | publicGames =
                                    ServerInterface.appendPublicGames
                                        publicGame
                                        state5.publicGames
                            }
                                |> bumpStatistic .totalPublicConnections
                in
                ( state6
                , Just <|
                    NewRsp
                        { gameid = gameid
                        , playerid = playerid
                        , player = 0
                        , name = name
                        , publicType = publicType
                        , gameState = gameState
                        , wasRestored = maybeGameid /= Nothing
                        }
                )

        JoinReq { gameid, name, isRestore } ->
            case ServerInterface.getGame gameid state of
                Nothing ->
                    errorRes message state "Unknown session id. Try again when a player has joined."

                Just gameState ->
                    let
                        players =
                            gameState.players

                        nextPlayer =
                            Dict.size players
                    in
                    if name == "" || (List.member name <| Dict.values players) then
                        errorRes message
                            state
                            ("Blank or existing name: \"" ++ name ++ "\"")

                    else if Dict.isEmpty players then
                        let
                            nameExists ids =
                                case ids of
                                    [] ->
                                        False

                                    id :: tail ->
                                        case ServerInterface.getPlayer id state of
                                            Nothing ->
                                                -- Can't happen
                                                nameExists tail

                                            Just _ ->
                                                False
                        in
                        if nameExists <| ServerInterface.getGamePlayers gameid state then
                            errorRes message state <|
                                "Already a player name: "
                                    ++ name

                        else
                            let
                                ( playerid, state2 ) =
                                    ServerInterface.newPlayerid state

                                participant =
                                    0

                                state3 =
                                    ServerInterface.addPlayer playerid
                                        { gameid = gameid
                                        , player = participant
                                        }
                                        state2
                            in
                            ( state3
                            , Just <|
                                JoinRsp
                                    { gameid = gameid
                                    , playerid = Just playerid
                                    , gameState = gameState
                                    , wasRestored = isRestore
                                    }
                            )

                    else
                        let
                            players2 =
                                Dict.insert nextPlayer name players

                            playerCount =
                                Dict.size players2

                            ( playerid, state2 ) =
                                ServerInterface.newPlayerid state

                            participant =
                                nextPlayer

                            state3 =
                                ServerInterface.addPlayer playerid
                                    { gameid = gameid
                                    , player = participant
                                    }
                                    state2

                            gameState2 =
                                { gameState | players = players2 }

                            state4 =
                                ServerInterface.updateGame gameid
                                    gameState2
                                    state3
                                    |> bumpStatistic .activeGames

                            removePublicGame =
                                case
                                    LE.find (\pg -> gameid == pg.gameid)
                                        state4.publicGames
                                of
                                    Nothing ->
                                        False

                                    Just pg ->
                                        -- Remove only undecodable and non-private "public" games.
                                        case ED.frameworkToPublicGame pg of
                                            Nothing ->
                                                True

                                            Just { forName } ->
                                                forName /= Nothing

                            state5 =
                                if not removePublicGame then
                                    state4

                                else
                                    { state4
                                        | publicGames =
                                            ServerInterface.removePublicGame
                                                gameid
                                                state4.publicGames
                                    }
                        in
                        ( state5
                        , Just <|
                            JoinRsp
                                { gameid = gameid
                                , playerid = Just playerid
                                , gameState = gameState2
                                , wasRestored = isRestore
                                }
                        )

        SetGameStateReq { playerid, gameState } ->
            if not isProxyServer && not WhichServer.isLocal then
                errorRes message state "SetGameStateReq is disabled."

            else
                let
                    body gameid player =
                        let
                            gs =
                                gameState
                                    |> populateWinner time

                            state2 =
                                populateEndOfGameStatistics gs state
                        in
                        ( ServerInterface.updateGame gameid gs state2
                        , Just <|
                            UpdateRsp
                                { gameid = gameid
                                , gameState = gs
                                }
                        )
                in
                case lookupGame message playerid state of
                    Err res ->
                        res

                    Ok ( gameid, _, participant ) ->
                        body gameid participant

        UpdateReq { playerid } ->
            case lookupGame message playerid state of
                Err res ->
                    res

                Ok ( gameid, gameState, _ ) ->
                    ( state
                    , Just <|
                        UpdateRsp
                            { gameid = gameid
                            , gameState = gameState
                            }
                    )

        PlayReq { playerid, placement } ->
            let
                body : GameId -> GameState -> Player -> ( Types.ServerState, Maybe Message )
                body gameid gameState player =
                    if
                        not isProxyServer
                            && (gameState.winner == NoWinner)
                            && (player /= gameState.player)
                    then
                        errorRes message state "It's not your turn."

                    else
                        let
                            board =
                                gameState.board
                        in
                        case placement of
                            ChooseTableau card ->
                                case Array.get player board.hands of
                                    Nothing ->
                                        -- Should be an error
                                        ( state, Nothing )

                                    Just cards ->
                                        let
                                            newTableau =
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
                                                    | tableau = newTableau
                                                    , hands =
                                                        Array.set player
                                                            ((card :: cards)
                                                                |> Board.sortCards
                                                            )
                                                            board.hands
                                                }

                                            newState : Types.State
                                            newState =
                                                if Board.isTableauEmpty newTableau then
                                                    TurnStockState

                                                else
                                                    gameState.state

                                            newGameState : GameState
                                            newGameState =
                                                { gameState
                                                    | board = newBoard
                                                    , state = newState
                                                }
                                        in
                                        ( { state | state = Just newGameState }
                                        , Just <|
                                            PlayRsp
                                                { gameid = gameid
                                                , gameState = newGameState
                                                }
                                        )

                            ChooseStock ->
                                if gameState.state /= ChooseStockState then
                                    errorRes message state "ChooseStock not allowed"

                                else
                                    let
                                        newGameState =
                                            setNextPlayer gameState
                                    in
                                    if gameState.player == gameState.whoseTurn then
                                        let
                                            newGameState2 =
                                                { newGameState
                                                    | whoseTurn =
                                                        newGameState.player
                                                    , state =
                                                        TurnStockState
                                                    , board =
                                                        { board
                                                            | turnedStock = Nothing
                                                        }
                                                }
                                        in
                                        ( { state
                                            | state = Just newGameState2
                                          }
                                        , Just <|
                                            PlayRsp
                                                { gameid = gameid
                                                , gameState = newGameState2
                                                }
                                        )

                                    else
                                        ( { state
                                            | state = Just newGameState
                                          }
                                        , Just <|
                                            PlayRsp
                                                { gameid = gameid
                                                , gameState = newGameState
                                                }
                                        )

                            SkipStock ->
                                ( state, Nothing )

                            Discard card ->
                                ( state, Nothing )

                            SayUncle ->
                                ( state, Nothing )

                            ChooseNew ->
                                case gameState.winner of
                                    NoWinner ->
                                        errorRes message state "Game not over"

                                    _ ->
                                        let
                                            playerCount =
                                                Dict.size gameState.players

                                            p =
                                                gameState.player + 1

                                            newPlayer =
                                                if p == playerCount then
                                                    0

                                                else
                                                    p

                                            newBoard =
                                                Board.initial playerCount
                                                    gameState.board.seed

                                            gs =
                                                { gameState
                                                    | board = newBoard
                                                    , whoseTurn = newPlayer
                                                    , player = newPlayer
                                                    , winner = NoWinner
                                                    , private = Types.emptyPrivateGameState
                                                }

                                            state2 =
                                                bumpStatistic .activeGames state
                                        in
                                        ( ServerInterface.updateGame gameid gs state2
                                        , Just <|
                                            AnotherGameRsp
                                                { gameid = gameid
                                                , gameState = gs
                                                , playerid = playerid
                                                }
                                        )
            in
            case lookupGame message playerid state of
                Err res ->
                    Debug.log "PlayReq Err" res

                Ok ( gameid, gameState, participant ) ->
                    body gameid gameState participant

        PublicGamesReq { subscribe, forName, gameid } ->
            -- subscribe is processed by the server code only
            let
                games =
                    state.publicGames
                        |> List.filterMap ED.frameworkToPublicGame
                        |> List.filterMap
                            (\game ->
                                let
                                    matches =
                                        forNameMatches forName game.forName
                                            || (Maybe.withDefault "" gameid
                                                    == game.gameid
                                               )

                                    include =
                                        matches || game.forName == Nothing
                                in
                                if include then
                                    Just
                                        { game
                                            | forName =
                                                if matches then
                                                    game.forName

                                                else
                                                    Nothing
                                        }

                                else
                                    Nothing
                            )
                        |> List.map (publicGameAddPlayers state)
            in
            ( state, Just <| PublicGamesRsp { games = games } )

        StatisticsReq { subscribe } ->
            -- subscription is processed by the server code only
            let
                statistics =
                    state.statistics

                ( startTime, updateTime ) =
                    case state.state of
                        Nothing ->
                            ( Nothing, Nothing )

                        Just gs ->
                            let
                                pgs =
                                    gs.private
                            in
                            ( pgs.startTime, pgs.updateTime )
            in
            ( state
            , Just <|
                StatisticsRsp
                    { statistics = statistics
                    , startTime = startTime
                    , updateTime = updateTime
                    }
            )

        ChatReq { playerid, text } ->
            case lookupGame message playerid state of
                Err res ->
                    res

                Ok ( gameid, gameState, participant ) ->
                    let
                        body name =
                            ( state
                            , Just <|
                                ChatRsp
                                    { gameid = gameid
                                    , name = name
                                    , text = text
                                    }
                            )

                        players =
                            Dict.toList gameState.players
                    in
                    case LE.find (\( p, _ ) -> p == participant) players of
                        Nothing ->
                            errorRes message state <|
                                "Player not found: "
                                    ++ String.fromInt participant

                        Just ( _, name ) ->
                            body name

        _ ->
            errorRes message state "Received a non-request."


publicGameAddPlayers : Types.ServerState -> PublicGame -> PublicGameAndPlayers
publicGameAddPlayers state publicGame =
    let
        players =
            case ServerInterface.getGame publicGame.gameid state of
                Nothing ->
                    Dict.fromList
                        [ ( publicGame.player, publicGame.creator ) ]

                Just gameState ->
                    gameState.players
    in
    { publicGame = publicGame
    , players = players
    }


getGameParticipants : GameId -> Types.ServerState -> List Participant
getGameParticipants gameid state =
    ServerInterface.getGamePlayers gameid state
        |> List.filterMap
            (\playerid ->
                case ServerInterface.getPlayer playerid state of
                    Nothing ->
                        Nothing

                    Just info ->
                        Just info.player
            )


populateEndOfGameStatistics : GameState -> Types.ServerState -> Types.ServerState
populateEndOfGameStatistics gameState state =
    let
        update _ =
            state
                |> bumpStatistic .finishedGames
                |> decrementStatistic .activeGames
    in
    case gameState.winner of
        NoWinner ->
            state

        _ ->
            update ()


winningPlayer : Array (List Card) -> Player
winningPlayer hands =
    let
        indexedHands =
            List.indexedMap (\idx hand -> ( idx, hand )) <| Array.toList hands

        folder : ( Player, List Card ) -> ( ( Int, Suit ), Player ) -> ( ( Int, Suit ), Player )
        folder ( player, cards ) ( ( winLen, winSuit ), winPlayer ) =
            let
                ( len, suit ) =
                    Board.longestStraightFlush cards
            in
            if
                (len > winLen)
                    || ((len == winLen) && Board.suitOrder suit winSuit == GT)
            then
                ( ( len, suit ), player )

            else
                ( ( winLen, winSuit ), winPlayer )

        ( _, resPlayer ) =
            List.foldr folder ( ( 0, Clubs ), 0 ) indexedHands
    in
    resPlayer


populateWinner : Posix -> GameState -> GameState
populateWinner time gameState =
    case gameState.winner of
        NoWinner ->
            let
                board =
                    gameState.board
            in
            if Deck.length board.stock == 0 && board.turnedStock == Nothing then
                { gameState
                    | winner = StockUsedWinner <| winningPlayer board.hands
                }

            else
                gameState

        _ ->
            gameState


updateScore : GameState -> GameState
updateScore gameState =
    let
        score =
            gameState.score

        names =
            gameState.players

        deltas : List ( Player, Int )
        deltas =
            case gameState.winner of
                NoWinner ->
                    []

                SayUncleWinner { saidUncle, won } ->
                    if won == saidUncle then
                        [ ( saidUncle, 2 ) ]

                    else
                        case Dict.get saidUncle score.points of
                            Nothing ->
                                []

                            Just points ->
                                if points == 0 then
                                    [ ( won, 2 ) ]

                                else
                                    [ ( saidUncle, points - 1 )
                                    , ( won, 1 )
                                    ]

                StockUsedWinner won ->
                    [ ( won, 1 ) ]
    in
    if gameState.winner == NoWinner then
        gameState

    else
        let
            folder : ( Player, Int ) -> Score -> Score
            folder ( player, delta ) score1 =
                let
                    points =
                        case Dict.get player score1.points of
                            Nothing ->
                                0

                            Just ppp ->
                                ppp
                in
                { score1
                    | points =
                        Dict.insert player (points + delta) score1.points
                }

            score2 =
                List.foldl folder score deltas
        in
        { gameState
            | score =
                { score2
                    | games = score2.games + 1
                }
        }
