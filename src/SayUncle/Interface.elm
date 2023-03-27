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
    , ensuringPlayer
    , forNameMatches
    , getStatisticsChanged
    , isFirstJumpTo
    , messageProcessor
    , proxyMessageProcessor
    , publicGameAddPlayers
    , setStatisticsChanged
    )

import Debug
import Dict exposing (Dict)
import List.Extra as LE
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


emptyGameState : PlayerNames -> Int -> Int -> GameState
emptyGameState players maxPlayers winningPoints =
    { board = Board.initial
    , maxPlayers = maxPlayers
    , winningPoints = winningPoints
    , players = players
    , whoseTurn = 0
    , player = 0
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


notForPeonsError : Message -> Types.ServerState -> String -> ( Types.ServerState, Maybe Message )
notForPeonsError message state messageName =
    errorRes message state <| messageName ++ " is not permitted to crowd participants."


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


ensuringPlayer : Participant -> (String -> res) -> (Player -> res) -> res
ensuringPlayer participant errorThunk thunk =
    case participant of
        PlayingParticipant player ->
            thunk player

        CrowdParticipant name ->
            errorThunk name


generalMessageProcessorInternal : Bool -> Types.ServerState -> Message -> ( Types.ServerState, Maybe Message )
generalMessageProcessorInternal isProxyServer state message =
    let
        time =
            state.time
    in
    case message of
        NewReq { name, publicType, maxPlayers, winningPoints, restoreState, maybeGameid } ->
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
                        Dict.fromList [ ( player, name ) ]

                    gameState =
                        case restoreState of
                            Nothing ->
                                emptyGameState players maxPlayers winningPoints

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
                        { gameid = gameid, player = PlayingParticipant player }

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
                                    , player = player
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
                        , player = player
                        , name = name
                        , publicType = publicType
                        , gameState = gameState
                        , wasRestored = maybeGameid /= Nothing
                        }
                )

        JoinReq { gameid, name, isRestore, inCrowd } ->
            case ServerInterface.getGame gameid state of
                Nothing ->
                    errorRes message state "Unknown session id. Try again when a player has joined."

                Just gameState ->
                    let
                        players =
                            gameState.players |> Dict.toList
                    in
                    if name == "" || List.any (\( _, n ) -> n == name) players then
                        errorRes message
                            state
                            ("Blank or existing name: \"" ++ name ++ "\"")

                    else if players == [] || inCrowd then
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

                                            Just playerInfo ->
                                                case playerInfo.player of
                                                    PlayingParticipant _ ->
                                                        -- Checked above
                                                        False

                                                    CrowdParticipant idname ->
                                                        if idname == name then
                                                            True

                                                        else
                                                            nameExists tail
                        in
                        if nameExists <| ServerInterface.getGamePlayers gameid state then
                            errorRes message state <|
                                "Already a participant name: "
                                    ++ name

                        else
                            let
                                ( playerid, state2 ) =
                                    ServerInterface.newPlayerid state

                                participant =
                                    CrowdParticipant name

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
                                    , participant = participant
                                    , gameState = gameState
                                    , wasRestored = isRestore
                                    }
                            )

                    else
                        let
                            player =
                                List.length players

                            players2 =
                                ( player, name ) :: players

                            ( playerid, state2 ) =
                                ServerInterface.newPlayerid state

                            participant =
                                PlayingParticipant player

                            state3 =
                                ServerInterface.addPlayer playerid
                                    { gameid = gameid
                                    , player = participant
                                    }
                                    state2

                            gameState2 =
                                { gameState | players = Dict.fromList players2 }

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
                                , participant = participant
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

                    err _ =
                        notForPeonsError message state "SetGameStateReq"
                in
                case lookupGame message playerid state of
                    Err res ->
                        res

                    Ok ( gameid, _, participant ) ->
                        ensuringPlayer participant err <| body gameid

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
                body gameid gameState player =
                    if
                        not isProxyServer
                            && (gameState.winner == NoWinner)
                            && (player /= gameState.player)
                    then
                        errorRes message state "It's not your turn."

                    else
                        case placement of
                            ChooseNew ->
                                case gameState.winner of
                                    NoWinner ->
                                        errorRes message state "Game not over"

                                    _ ->
                                        let
                                            players =
                                                if player == newPlayer then
                                                    gameState.players

                                                else
                                                    let
                                                        { white, black } =
                                                            gameState.players
                                                    in
                                                    { white = black
                                                    , black = white
                                                    }

                                            gs =
                                                emptyGameState players

                                            gs2 =
                                                { gs
                                                    | score = gameState.score
                                                    , requestUndo = NoRequestUndo
                                                }

                                            state2 =
                                                if player == newPlayer then
                                                    state

                                                else
                                                    let
                                                        playerids =
                                                            ServerInterface.getGamePlayers
                                                                gameid
                                                                state

                                                        loop : PlayerId -> Types.ServerState -> Types.ServerState
                                                        loop id st =
                                                            let
                                                                pl =
                                                                    if id == playerid then
                                                                        newPlayer

                                                                    else
                                                                        Types.otherPlayer
                                                                            newPlayer

                                                                participant =
                                                                    PlayingParticipant pl
                                                            in
                                                            ServerInterface.updatePlayer
                                                                id
                                                                { gameid = gameid
                                                                , player = participant
                                                                }
                                                                st
                                                    in
                                                    List.foldl loop state playerids

                                            state3 =
                                                bumpStatistic .activeGames state2
                                        in
                                        ( ServerInterface.updateGame gameid gs2 state3
                                        , Just <|
                                            AnotherGameRsp
                                                { gameid = gameid
                                                , gameState = gs2
                                                , player = newPlayer
                                                }
                                        )

                err _ =
                    notForPeonsError message state "PlayReq"
            in
            case lookupGame message playerid state of
                Err res ->
                    Debug.log "PlayReq Err" res

                Ok ( gameid, gameState, participant ) ->
                    ensuringPlayer participant err <| body gameid gameState

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
                    in
                    case participant of
                        CrowdParticipant name ->
                            body name

                        PlayingParticipant player ->
                            let
                                players =
                                    gameState.players

                                name =
                                    case player of
                                        WhitePlayer ->
                                            players.white

                                        BlackPlayer ->
                                            players.black
                            in
                            body name

        _ ->
            errorRes message state "Received a non-request."


publicGameAddPlayers : Types.ServerState -> PublicGame -> PublicGameAndPlayers
publicGameAddPlayers state publicGame =
    let
        ( moves, players ) =
            case ServerInterface.getGame publicGame.gameid state of
                Nothing ->
                    ( []
                    , { white = "White"
                      , black = ""
                      }
                    )

                Just gameState ->
                    ( gameState.moves
                    , gameState.players
                    )

        ( startTime, endTime ) =
            case List.head moves of
                Nothing ->
                    ( Types.posixZero, Types.posixZero )

                Just lastMove ->
                    case List.head <| List.reverse moves of
                        Nothing ->
                            -- Can't happen
                            ( lastMove.time, lastMove.time )

                        Just firstMove ->
                            ( firstMove.time, lastMove.time )
    in
    { publicGame = publicGame
    , players = players
    , watchers =
        getCrowdParticipants publicGame.gameid state
            |> List.length
    , moves = List.length moves
    , startTime = startTime
    , endTime = endTime
    }


isCrowdParticipant : Participant -> Bool
isCrowdParticipant participant =
    case participant of
        CrowdParticipant _ ->
            True

        _ ->
            False


isPlayingParticipant : Participant -> Bool
isPlayingParticipant =
    isCrowdParticipant >> not


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


getCrowdParticipants : GameId -> Types.ServerState -> List Participant
getCrowdParticipants gameid state =
    getGameParticipants gameid state
        |> List.filter isCrowdParticipant


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
            indexedMap (\idx hand -> ( idx, hand )) <| Array.toList hands

        folder : ( Player, List Card ) -> ( ( Int, Suit ), Player ) -> ( ( Int, Suit ), Player )
        folder ( player, cards ) ( ( winLen, winSuit ), winPlayer ) =
            let
                ( len, suit ) =
                    Board.longestStraigtFlush cards
            in
            if
                len
                    > winLen
                    || ((len == winLen) && suitOrder suit winSuit == GT)
            then
                ( ( len, suit ), player )

            else
                ( ( winLen, winSuit ), winPlayer )

        ( _, resPlayer ) =
            List.foldr folder ( 0, Clubs ) indexedHands
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
