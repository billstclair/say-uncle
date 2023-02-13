--------------------------------------------------------------------
--
-- Interface.elm
-- Say Uncle server interface.
-- Runs on local machine for local play, and server for networked play.
-- Copyright (c) 2019-2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module SayUncle.Interface exposing
    ( archiveGame
    , bumpStatistic
    , emptyGameState
    , ensuringPlayer
    , forNameMatches
    , getStatisticsChanged
    , isFirstJumpTo
    , messageProcessor
    , proxyMessageProcessor
    , publicGameAddPlayers
    , setStatisticsChanged
    , unarchiveGame
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
        , Participant(..)
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


emptyGameState : PlayerNames -> GameState
emptyGameState players =
    { newBoard = Board.initial
    , initialBoard = Nothing
    , moves = []
    , players = players
    , whoseTurn = WhitePlayer
    , selected = Nothing
    , jumperLocations = []
    , legalMoves = NoMoves
    , undoStates = []
    , jumps = []
    , score = Types.zeroScore
    , winner = NoWinner
    , requestUndo = NoRequestUndo
    , testMode = Nothing
    , testModeInitialState = Nothing
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
        NewReq { name, player, publicType, gamename, restoreState, maybeGameid } ->
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
                        case player of
                            WhitePlayer ->
                                { white = name
                                , black = ""
                                }

                            BlackPlayer ->
                                { white = ""
                                , black = name
                                }

                    gameState =
                        case restoreState of
                            Nothing ->
                                emptyGameState players

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
                        , gamename = gamename
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
                            gameState.players

                        { white, black } =
                            players
                    in
                    if name == "" || name == white || name == black then
                        errorRes message
                            state
                            ("Blank or existing name: \"" ++ name ++ "\"")

                    else if (white /= "" && black /= "") || inCrowd then
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
                            ( players2, player ) =
                                if white == "" then
                                    ( { players | white = name }, WhitePlayer )

                                else
                                    ( { players | black = name }, BlackPlayer )

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
                                , participant = participant
                                , gameState = gameState2
                                , wasRestored = isRestore
                                }
                        )

        LeaveReq { playerid } ->
            let
                body gameid gameState participant player =
                    let
                        players =
                            gameState.players

                        gameOn =
                            players.white /= "" && players.black /= ""

                        state2 =
                            ServerInterface.removeGame gameid state
                                |> decrementStatistic .activeConnections
                                |> (if gameOn && gameState.winner == NoWinner then
                                        decrementStatistic .activeGames

                                    else
                                        identity
                                   )
                    in
                    ( state2
                    , Just <|
                        LeaveRsp
                            { gameid = gameid
                            , participant = participant
                            }
                    )

                err gameid participant _ =
                    ( ServerInterface.removePlayer playerid state
                    , Just <|
                        LeaveRsp
                            { gameid = gameid
                            , participant = participant
                            }
                    )
            in
            case lookupGame message playerid state of
                Err res ->
                    res

                Ok ( gameid, gameState, participant ) ->
                    ensuringPlayer participant (err gameid participant) <|
                        body gameid gameState participant

        SetGameStateReq { playerid, gameState } ->
            if not isProxyServer && not WhichServer.isLocal then
                errorRes message state "SetGameStateReq is disabled."

            else
                let
                    body gameid player =
                        let
                            gs =
                                gameState
                                    |> updateJumperLocations
                                    |> Board.populateLegalMoves
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


updateJumperLocations : GameState -> GameState
updateJumperLocations gameState =
    let
        jumperLocations =
            Board.computeJumperLocations
                (Types.playerColor gameState.whoseTurn)
                gameState.newBoard
    in
    { gameState | jumperLocations = jumperLocations }


processChooseMoveOption : ChooseMoveOption -> RowCol -> Bool -> Player -> Maybe ( RowCol, Piece ) -> GameState -> ( GameState, Maybe String )
processChooseMoveOption option moveTo lastMove whoseTurn jumpOver gameState =
    let
        board =
            gameState.newBoard

        selectedPiece =
            Board.get moveTo board

        ( selectedType, selectedColor ) =
            ( selectedPiece.pieceType, selectedPiece.color )
    in
    case option of
        NoOption ->
            ( gameState, Nothing )

        CorruptJumped ->
            case jumpOver of
                Nothing ->
                    ( gameState, Just "Can't corrupt a jumped piece on a non-jump." )

                Just ( jumpedPos, piece ) ->
                    let
                        { pieceType, color } =
                            piece
                    in
                    if selectedType /= Journeyman then
                        ( gameState, Just "Only a Journeyman may corrupt a piece." )

                    else if pieceType /= Golem && pieceType /= Hulk then
                        ( gameState, Just "Can't corrupt a Journeyman or a Corrupted Hulk" )

                    else if colorMatchesPlayer color whoseTurn then
                        ( gameState, Just "Can't corrupt a piece of your own color." )

                    else if
                        (pieceType == Golem)
                            && (Board.countColor color board >= 23)
                    then
                        -- This can happen, but I'll bet it won't.
                        -- Sorta like the two-move mate.
                        ( gameState, Just "There are no units to use to make a corrupted hulk." )

                    else
                        let
                            ( newBoard, jumps ) =
                                if gameState.jumps == [] then
                                    let
                                        corruptedHulk =
                                            { pieceType = CorruptedHulk
                                            , color = Types.otherColor color
                                            }
                                    in
                                    ( Board.set jumpedPos corruptedHulk board
                                    , gameState.jumps
                                    )

                                else
                                    ( board
                                    , LE.updateIf
                                        (.over >> (==) jumpedPos)
                                        (\jump ->
                                            { jump
                                                | hulkAfterJump =
                                                    CorruptAfterJump
                                            }
                                        )
                                        gameState.jumps
                                    )
                        in
                        ( { gameState
                            | newBoard = newBoard
                            , jumps = jumps
                          }
                        , Nothing
                        )

        MakeHulk hulkPos ->
            let
                hulkPiece =
                    Board.get hulkPos board
            in
            if not lastMove then
                ( gameState, Just "May only make a hulk if your final jump lands on the other player's sanctum." )

            else if
                ((selectedColor == WhiteColor) && (moveTo /= Board.blackSanctum))
                    || ((selectedColor == BlackColor) && (moveTo /= Board.whiteSanctum))
            then
                ( gameState, Just "Can't make a hulk except by landing on the other player's sanctum." )

            else if
                (selectedType /= Golem)
                    && (selectedType /= Hulk)
                    && (selectedType /= CorruptedHulk)
            then
                ( gameState, Just "Can't make a Hulk from a Journeyman" )

            else if hulkPiece.pieceType /= Golem then
                let
                    p =
                        Debug.log "processChooseMoveOption, (hulkPos, hulkPiece)" ( hulkPos, hulkPiece )
                in
                ( gameState, Just "Can only convert a Golem to a hulk." )

            else if selectedColor /= hulkPiece.color then
                ( gameState, Just "Can't convert another player's piece to a hulk." )

            else if gameState.undoStates /= [] then
                -- There are jumps left
                ( gameState, Just "Can only make a hulk if the sanctum is the final location." )

            else if moveTo == hulkPos then
                ( gameState, Just "Can't convert the jumper to a hulk." )

            else
                let
                    hulk =
                        { pieceType = Hulk
                        , color = selectedColor
                        }

                    newBoard =
                        Board.clear moveTo board
                            |> Board.set hulkPos hulk
                in
                ( { gameState | newBoard = newBoard }, Nothing )


chooseMoveOptionToHulkAfterJump : ChooseMoveOption -> HulkAfterJump
chooseMoveOptionToHulkAfterJump option =
    case option of
        NoOption ->
            NoHulkAfterJump

        CorruptJumped ->
            CorruptAfterJump

        MakeHulk hulkPos ->
            MakeHulkAfterJump hulkPos


toCorruptibleJump : RowCol -> ChooseMoveOption -> OneJump -> OneCorruptibleJump
toCorruptibleJump from option { over, to } =
    { from = from
    , over = over
    , to = to
    , hulkAfterJump = chooseMoveOptionToHulkAfterJump option
    }


populateEndOfGameStatistics : GameState -> Types.ServerState -> Types.ServerState
populateEndOfGameStatistics gameState state =
    let
        update player =
            state
                |> bumpStatistic .finishedGames
                |> decrementStatistic .activeGames
                |> bumpStatistic
                    (case player of
                        WhitePlayer ->
                            .whiteWon

                        BlackPlayer ->
                            .blackWon
                    )
                -- Ensure that there is a row for each on the statistics page.
                |> changeStatistic
                    (case player of
                        WhitePlayer ->
                            .blackWon

                        BlackPlayer ->
                            .whiteWon
                    )
                    0
                |> changeStatistic .totalMoves (List.length gameState.moves)
    in
    case gameState.winner of
        NoWinner ->
            state

        WhiteWinner _ ->
            update WhitePlayer

        BlackWinner _ ->
            update BlackPlayer


populateWinner : Posix -> GameState -> GameState
populateWinner time gameState =
    case gameState.winner of
        NoWinner ->
            let
                winner =
                    Board.computeWinner gameState.whoseTurn gameState.newBoard
            in
            { gameState | winner = winner }
                |> populateWinnerInFirstMove time

        _ ->
            gameState


populateWinnerInFirstMove : Posix -> GameState -> GameState
populateWinnerInFirstMove time gameState =
    let
        winner =
            gameState.winner

        ( moves, reason, otherColor ) =
            case winner of
                NoWinner ->
                    ( gameState.moves, WinByCapture, WhiteColor )

                WhiteWinner reas ->
                    ( gameState.moves, reas, BlackColor )

                BlackWinner reas ->
                    ( gameState.moves, reas, WhiteColor )

        newMoves =
            if winner == NoWinner then
                moves

            else
                case reason of
                    WinByResignation ->
                        { piece =
                            { color = otherColor
                            , pieceType = Golem
                            }
                        , isUnique = True
                        , sequence = OneResign
                        , winner = winner
                        , time = time
                        }
                            :: moves

                    _ ->
                        case moves of
                            move :: otherMoves ->
                                { move | winner = winner } :: otherMoves

                            ms ->
                                ms
    in
    { gameState | moves = newMoves }
        |> updateScore


chooseMove : Types.ServerState -> Message -> String -> GameState -> Player -> RowCol -> ChooseMoveOption -> ( Types.ServerState, Maybe Message )
chooseMove state message gameid gameState player rowCol option =
    let
        time =
            state.time

        board =
            gameState.newBoard
    in
    case gameState.selected of
        Nothing ->
            errorRes message state "No selected piece."

        Just selected ->
            let
                legalMoves =
                    gameState.legalMoves

                piece =
                    Board.get selected board
            in
            case legalMoves of
                NoMoves ->
                    errorRes message state "There are no legal moves."

                Moves rowCols ->
                    if not <| List.member rowCol rowCols then
                        errorRes message state "Not a legal move."

                    else
                        let
                            isUnique =
                                Board.isUniqueMoveTo selected
                                    rowCol
                                    (Just piece)
                                    False
                                    board

                            move =
                                { piece = piece
                                , isUnique = isUnique
                                , sequence =
                                    OneSlide
                                        { from = selected
                                        , to = rowCol
                                        , makeHulk =
                                            case option of
                                                MakeHulk hulkPos ->
                                                    Just hulkPos

                                                _ ->
                                                    Nothing
                                        }
                                , winner = NoWinner
                                , time = time
                                }

                            gs =
                                { gameState
                                    | moves = move :: gameState.moves
                                    , requestUndo = NoRequestUndo
                                }
                                    |> endOfTurn selected rowCol piece option

                            ( gs2, maybeError ) =
                                processChooseMoveOption option rowCol True gameState.whoseTurn Nothing gs
                        in
                        case maybeError of
                            Just err ->
                                errorRes message state err

                            Nothing ->
                                let
                                    gs3 =
                                        gs2
                                            |> updateJumperLocations
                                            |> populateWinner time

                                    state2 =
                                        populateEndOfGameStatistics gs3 state
                                in
                                ( ServerInterface.updateGame gameid gs3 state2
                                , Just <|
                                    PlayRsp
                                        { gameid = gameid
                                        , gameState = gs3
                                        }
                                )

                Jumps sequences ->
                    let
                        remaining =
                            List.filter (isFirstJumpTo rowCol) sequences

                        newSequences =
                            List.map (List.drop 1) remaining
                    in
                    case remaining of
                        [] ->
                            errorRes message state "Not a legal jump."

                        firstSequence :: _ ->
                            let
                                firstOver =
                                    case List.head firstSequence of
                                        Nothing ->
                                            -- Can't happen
                                            Board.rc -1 -1

                                        Just { over } ->
                                            over

                                jumpOver =
                                    Just ( firstOver, Board.get firstOver board )

                                newMove =
                                    { from = selected
                                    , over = firstOver
                                    , to = rowCol
                                    , hulkAfterJump =
                                        chooseMoveOptionToHulkAfterJump option
                                    }

                                moves =
                                    case gameState.jumps of
                                        _ :: _ ->
                                            case gameState.moves of
                                                [] ->
                                                    -- Can't happen
                                                    []

                                                move :: otherMoves ->
                                                    case move.sequence of
                                                        OneResign ->
                                                            -- Can't happen
                                                            []

                                                        OneSlide _ ->
                                                            -- Can't happen
                                                            []

                                                        OneJumpSequence jumps ->
                                                            let
                                                                sequence =
                                                                    OneJumpSequence <|
                                                                        jumps
                                                                            ++ [ newMove ]
                                                            in
                                                            { move
                                                                | sequence =
                                                                    sequence
                                                            }
                                                                :: otherMoves

                                        _ ->
                                            -- First jump
                                            let
                                                isUnique =
                                                    Board.isUniqueMoveTo selected
                                                        rowCol
                                                        (Just piece)
                                                        True
                                                        board
                                            in
                                            { piece = piece
                                            , isUnique = isUnique
                                            , sequence =
                                                OneJumpSequence [ newMove ]
                                            , winner = NoWinner
                                            , time = time
                                            }
                                                :: gameState.moves
                            in
                            case List.head newSequences of
                                Nothing ->
                                    -- Can't happen
                                    errorRes message state "No jump sequences."

                                Just [] ->
                                    -- End of jumps
                                    let
                                        doJump jump board2 =
                                            case jump.hulkAfterJump of
                                                CorruptAfterJump ->
                                                    Board.set jump.over
                                                        { color = piece.color
                                                        , pieceType = CorruptedHulk
                                                        }
                                                        board2

                                                _ ->
                                                    -- MakeHulkAfterJump is done
                                                    -- by processChooseMoveOption
                                                    Board.set jump.over
                                                        { color = piece.color
                                                        , pieceType = NoPiece
                                                        }
                                                        board2

                                        jumps =
                                            case List.head firstSequence of
                                                Nothing ->
                                                    -- can't happen
                                                    gameState.jumps

                                                Just jump ->
                                                    toCorruptibleJump selected option jump :: gameState.jumps

                                        newBoard =
                                            List.foldr doJump board jumps

                                        gs =
                                            { gameState
                                                | newBoard = newBoard
                                                , moves = moves
                                                , requestUndo = NoRequestUndo
                                            }

                                        gs2 =
                                            endOfTurn selected rowCol piece option gs

                                        ( gs3, maybeError ) =
                                            processChooseMoveOption option
                                                rowCol
                                                True
                                                gameState.whoseTurn
                                                jumpOver
                                                gs2
                                    in
                                    case maybeError of
                                        Just err ->
                                            errorRes message state err

                                        Nothing ->
                                            let
                                                gs4 =
                                                    gs3
                                                        |> updateJumperLocations
                                                        |> populateWinner time

                                                state2 =
                                                    populateEndOfGameStatistics gs4 state
                                            in
                                            ( ServerInterface.updateGame gameid gs4 state2
                                            , Just <|
                                                PlayRsp
                                                    { gameid = gameid
                                                    , gameState = gs4
                                                    }
                                            )

                                _ ->
                                    let
                                        gs =
                                            { gameState
                                                | newBoard =
                                                    Board.set selected Types.emptyPiece board
                                                        |> Board.set rowCol piece
                                                , moves = moves
                                                , requestUndo = NoRequestUndo
                                                , selected = Just rowCol
                                                , legalMoves = Jumps newSequences
                                                , undoStates =
                                                    { board = board
                                                    , moves = gameState.moves
                                                    , legalMoves = legalMoves
                                                    , selected = gameState.selected
                                                    }
                                                        :: gameState.undoStates
                                                , jumps =
                                                    (List.take 1 firstSequence
                                                        |> List.map (toCorruptibleJump rowCol option)
                                                    )
                                                        ++ gameState.jumps
                                            }

                                        ( gs2, maybeError ) =
                                            processChooseMoveOption option
                                                rowCol
                                                False
                                                gameState.whoseTurn
                                                jumpOver
                                                gs
                                    in
                                    case maybeError of
                                        Just err ->
                                            errorRes message state err

                                        Nothing ->
                                            ( ServerInterface.updateGame gameid gs2 state
                                            , Just <|
                                                PlayRsp
                                                    { gameid = gameid
                                                    , gameState = gs2
                                                    }
                                            )


endOfTurn : RowCol -> RowCol -> Piece -> ChooseMoveOption -> GameState -> GameState
endOfTurn selected moved piece option gameState =
    let
        whoseTurn =
            Types.otherPlayer gameState.whoseTurn

        isMakeHulk chooseMoveOption =
            case chooseMoveOption of
                MakeHulk _ ->
                    True

                _ ->
                    False

        setPiece =
            if
                (piece.pieceType /= Journeyman)
                    && (moved == Board.playerSanctum whoseTurn)
                    && (not <| isMakeHulk option)
            then
                Types.emptyPiece

            else
                piece

        newBoard =
            Board.clear selected gameState.newBoard
                |> Board.set moved setPiece
    in
    { gameState
        | newBoard = newBoard
        , selected = Nothing
        , legalMoves = NoMoves
        , whoseTurn = whoseTurn
        , undoStates = []
        , jumps = []
    }


chooseUndoJump : Types.ServerState -> Message -> String -> GameState -> UndoWhichJumps -> ( Types.ServerState, Maybe Message )
chooseUndoJump state message gameid gameState undoWhichJumps =
    case undoWhichJumps of
        UndoAllJumps ->
            let
                dropCnt =
                    List.length gameState.undoStates - 1

                gs =
                    { gameState
                        | undoStates =
                            List.drop dropCnt gameState.undoStates
                        , jumps =
                            List.drop dropCnt gameState.jumps
                    }
            in
            chooseUndoJump state message gameid gs UndoOneJump

        UndoOneJump ->
            case gameState.undoStates of
                [] ->
                    errorRes message state "There is nothing to undo."

                undoState :: undoStates ->
                    let
                        gs =
                            { gameState
                                | newBoard = undoState.board
                                , moves = undoState.moves
                                , selected = undoState.selected
                                , legalMoves = undoState.legalMoves
                                , undoStates = undoStates
                                , jumps = List.drop 1 gameState.jumps
                            }
                    in
                    ( ServerInterface.updateGame gameid gs state
                    , Just <|
                        PlayRsp
                            { gameid = gameid
                            , gameState = gs
                            }
                    )


isFirstJumpTo : RowCol -> JumpSequence -> Bool
isFirstJumpTo rowCol sequence =
    case List.head sequence of
        Nothing ->
            False

        Just oneJump ->
            oneJump.to == rowCol


colorMatchesPlayer : Color -> Player -> Bool
colorMatchesPlayer color player =
    if color == WhiteColor then
        player == WhitePlayer

    else
        player == BlackPlayer


cellName : ( Int, Int ) -> String
cellName ( rowidx, colidx ) =
    Board.colToString colidx ++ Board.rowToString rowidx


updateScore : GameState -> GameState
updateScore gameState =
    let
        score =
            gameState.score

        names =
            gameState.players

        ( winnerName, deltas ) =
            case gameState.winner of
                NoWinner ->
                    ( "", ( 0, 0 ) )

                WhiteWinner _ ->
                    ( names.white, ( 1, 0 ) )

                BlackWinner _ ->
                    ( names.black, ( 0, 1 ) )
    in
    if gameState.winner == NoWinner then
        gameState

    else
        let
            ( dw, db ) =
                deltas
        in
        { gameState
            | score =
                { score
                    | games = score.games + 1
                    , whiteWins = score.whiteWins + dw
                    , blackWins = score.blackWins + db
                }
        }


archiveGame : GameState -> ArchivedGame
archiveGame { moves, players, winner, initialBoard } =
    ArchivedGame moves players winner initialBoard


unarchiveGame : ArchivedGame -> GameState -> GameState
unarchiveGame { moves, players, winner, initialBoard } gameState =
    let
        ( firstTurn, aboard ) =
            case initialBoard of
                Nothing ->
                    ( WhitePlayer, Board.initial )

                Just iBoard ->
                    ( iBoard.whoseTurn, iBoard.board )

        ( whoseTurn, board ) =
            replayMoves moves firstTurn aboard
    in
    { gameState
        | newBoard = board
        , initialBoard = initialBoard
        , moves = moves
        , players = players
        , whoseTurn = whoseTurn
        , selected = Nothing
        , jumperLocations = []
        , legalMoves = NoMoves
        , undoStates = []
        , jumps = []
        , winner = winner
        , testMode = Nothing
    }
        |> updateJumperLocations


replayMoves : List OneMove -> Player -> Board -> ( Player, Board )
replayMoves moves whoseTurn board =
    -- Should this check that the moves are legal?
    -- List.foldr walks in reverse order, as intended.
    List.foldr replayMove ( whoseTurn, board ) moves


replayMove : OneMove -> ( Player, Board ) -> ( Player, Board )
replayMove { piece, sequence } ( whoseTurn, board ) =
    case sequence of
        OneResign ->
            ( whoseTurn, board )

        OneSlide { from, to, makeHulk } ->
            let
                board2 =
                    Board.set from Types.emptyPiece board
            in
            ( Types.otherPlayer whoseTurn
            , case makeHulk of
                Nothing ->
                    Board.set to piece board2

                Just hulkPos ->
                    Board.set to Types.emptyPiece board2
                        |> Board.set hulkPos
                            { color = Types.playerColor whoseTurn
                            , pieceType = Hulk
                            }
            )

        OneJumpSequence jumps ->
            ( Types.otherPlayer whoseTurn
            , List.foldl (replayOneCorruptibleJump whoseTurn piece) board jumps
            )


replayOneCorruptibleJump : Player -> Piece -> OneCorruptibleJump -> Board -> Board
replayOneCorruptibleJump whoseTurn piece { from, over, to, hulkAfterJump } board =
    board
        |> Board.set from Types.emptyPiece
        |> Board.set to piece
        |> (case hulkAfterJump of
                NoHulkAfterJump ->
                    Board.set over
                        Types.emptyPiece

                CorruptAfterJump ->
                    Board.set over
                        { color = Types.playerColor whoseTurn
                        , pieceType = CorruptedHulk
                        }

                MakeHulkAfterJump hulkPos ->
                    Board.set to Types.emptyPiece
                        >> Board.set hulkPos
                            { color = Types.playerColor whoseTurn
                            , pieceType = Hulk
                            }
           )
