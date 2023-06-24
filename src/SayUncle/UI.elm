---------------------------------------------------------------------
--
-- UI.elm
-- User Interface for Say Uncle.
-- Copyright (c) 2019-2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module SayUncle.UI exposing (view)

import DateFormat
import DateFormat.Relative
import Dict exposing (Dict)
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
import List.Extra as LE
import SayUncle.Types as Types
    exposing
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
        , Message(..)
        , MessageQueueEntry
        , Model
        , Msg(..)
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
import Time exposing (Month(..), Posix, Zone)


br : Html Msg
br =
    Html.br [] []


boardSize : Model -> Int
boardSize model =
    let
        ( w, h ) =
            model.windowSize
    in
    min (90 * w) (65 * h) // 100


herculanumStyle : Attribute msg
herculanumStyle =
    style "font-family" "Herculanum, sans-serif"


view : Model -> Document Msg
view model =
    let
        bsize =
            boardSize model

        settings =
            model.settings

        renderStyle =
            Types.typeToStyle model.styleType
    in
    { title = "Say Uncle"
    , body =
        [ if bsize == 0 then
            text ""

          else
            div
                [ style "background" renderStyle.backgroundColor
                , style "color" renderStyle.lineColor
                , style "padding" "5px"
                ]
                [ if settings.hideTitle then
                    text ""

                  else
                    div
                        [ align "center"
                        ]
                        [ h1
                            [ style "margin" "0 0 0.2em 0"
                            , herculanumStyle
                            ]
                            [ text "A•G•O•G" ]
                        , h2
                            [ style "margin" "0 0 0.2em 0"
                            , herculanumStyle
                            ]
                            [ text "A Game of Golems" ]
                        , p [ style "margin" "0" ]
                            [ text "Designed by Christopher St. Clair" ]
                        ]
                , case model.page of
                    MainPage ->
                        mainPage bsize model

                    RulesPage ->
                        rulesPage bsize model

                    InstructionsPage ->
                        instructionsPage bsize model

                    PublicPage ->
                        publicPage bsize model

                    MovesPage ->
                        movesPage bsize model

                    StatisticsPage ->
                        statisticsPage bsize model
                ]
        ]
    }


ids =
    { chatOutput = "chatOutput"
    , chatInput = "chatInput"
    , forName = "forName"
    }


winReasonToDescription : WinReason -> String
winReasonToDescription reason =
    case reason of
        WinByStackUsed ->
            "by stack exhausted"

        WinBySayUncle ->
            "by Say Uncle"


maybeNoText : String -> String
maybeNoText msg =
    if msg == "" then
        "[no text]"

    else
        msg


messageQueueDiv : Style -> Model -> Html Msg
messageQueueDiv theStyle model =
    let
        gameIsLocal =
            model.game.isLocal

        messages =
            model.messageQueue
                |> Fifo.toList
                |> List.reverse
                |> List.filter (\{ isLocal } -> isLocal == gameIsLocal)

        tableRow { isSend, message } =
            let
                sendText =
                    if isSend then
                        "sent: "

                    else
                        "rcvd: "

                messageText =
                    message
                        |> WSFED.encodeMessage ED.messageEncoder
            in
            tr []
                [ Html.th [ style "vertical-align" "top" ]
                    [ text sendText ]
                , td [] [ text messageText ]
                ]
    in
    if messages == [] then
        text ""

    else
        div
            [ align "center"
            , style "overflow-y" "scroll"
            , style "border" ("1px solid " ++ theStyle.lineColor)
            , style "resize" "vertical"
            , style "height" "12em"
            ]
            [ table [] <|
                List.map tableRow messages
            ]


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


mainPage : Int -> Model -> Html Msg
mainPage bsize model =
    let
        settings =
            model.settings

        game =
            model.game

        gameState =
            game.gameState

        players =
            liveGameState.players

        hasAllPlayers =
            Dict.size players == gameState.maxPlayers

        score =
            gameState.score

        ( playing, message, yourTurn ) =
            if not game.isLive then
                ( False
                , "Enter \"Your Name\" and either click \"Start Session\" or enter \"Session ID\" and click \"Join\""
                , True
                )

            else if not hasAllPlayers then
                ( False
                , "Waiting for other players to join"
                , False
                )

            else
                let
                    winString player maybeSaidUncle reason =
                        let
                            name =
                                localizedPlayerName player game
                        in
                        case saidUncle of
                            Nothing ->
                                name ++ " won " ++ winReasonToDescription reason ++ "!"

                            Just saidUncle ->
                                if saidUncle == player then
                                    name ++ " won after saying Uncle!"

                                else
                                    let
                                        saidUncleName =
                                            localizedPlayerName saidUncle game
                                    in
                                    name
                                        ++ " won after "
                                        ++ saidUncleName
                                        ++ " said Uncle!"
                in
                case gameState.winner of
                    StockUsedWinner player ->
                        ( False, winString player Nothing reason, False )

                    SayUncleWinner { saidUncle, won } ->
                        ( False, winString won (Just saidUncle) reason, False )

                    NoWinner ->
                        let
                            playString =
                                playDescription gameState.state

                            ( prefixp, action, yourTurn2 ) =
                                if
                                    not game.isLocal
                                        && (gameState.whoseTurn /= game.player)
                                then
                                    let
                                        otherName =
                                            localizedPlayerName gameState.whoseTurn
                                                game
                                    in
                                    ( False
                                    , "Waiting for "
                                        ++ otherName
                                        ++ " to "
                                        ++ playDescription
                                    , False
                                    )

                                else
                                    ( True
                                    , playString
                                    , True
                                    )

                            prefix =
                                if prefixp then
                                    let
                                        name =
                                            localizedPlayerName game.player game
                                    in
                                    name ++ ", "

                                else
                                    ""
                        in
                        ( True
                        , if action == "" then
                            ""

                          else
                            prefix ++ action ++ "."
                        , yourTurn2
                        )

        theStyle =
            Types.typeToStyle model.styleType

        messageStyles =
            notificationStyles yourTurn playing gameState

        movesSpan =
            let
                ( moveText, moveCnt, moveIndex ) =
                    moveString model
            in
            if moveCnt == 0 && model.showMove == Nothing then
                if not isArchiving then
                    text ""

                else
                    button [ onClick <| SetArchiveIndex "-1" ]
                        [ text "End archive" ]

            else
                let
                    threeSpaces =
                        chars.nbsp ++ chars.nbsp ++ chars.nbsp

                    boldWeight =
                        style "font-weight" "bold"
                in
                span []
                    [ a
                        [ href "#"
                        , onClick <| SetPage MovesPage
                        ]
                        [ b "Moves:" ]
                    , text " "
                    , text moveText
                    , if not isReviewingOrArchiving then
                        text ""

                      else
                        span []
                            [ text " "
                            , if isArchiving then
                                button [ onClick <| SetArchiveIndex "-1" ]
                                    [ text "End archive" ]

                              else
                                button [ onClick <| SetMoveIndex 0 ]
                                    [ text "End review" ]
                            ]
                    , br
                    , span [ boldWeight ]
                        [ if moveIndex < moveCnt then
                            span []
                                [ a
                                    [ href "#"
                                    , onClick <| SetMoveIndex moveCnt
                                    ]
                                    [ text <| chars.nbsp ++ "|<" ++ chars.nbsp ]
                                , text threeSpaces
                                , a
                                    [ href "#"
                                    , onClick <| SetMoveIndex (moveIndex + 1)
                                    ]
                                    [ text <| chars.nbsp ++ "<" ++ chars.nbsp ]
                                ]

                          else
                            span []
                                [ text <| chars.nbsp ++ "|<" ++ chars.nbsp
                                , text threeSpaces
                                , text <| chars.nbsp ++ "<" ++ chars.nbsp
                                ]
                        , text threeSpaces
                        , if moveIndex > 0 then
                            span []
                                [ a
                                    [ href "#"
                                    , onClick <| SetMoveIndex (moveIndex - 1)
                                    ]
                                    [ text <| chars.nbsp ++ ">" ++ chars.nbsp ]
                                , text threeSpaces
                                , a
                                    [ href "#"
                                    , onClick <| SetMoveIndex 0
                                    ]
                                    [ text <| chars.nbsp ++ ">|" ++ chars.nbsp ]
                                ]

                          else
                            span []
                                [ text <| chars.nbsp ++ ">" ++ chars.nbsp
                                , text threeSpaces
                                , text <| chars.nbsp ++ ">|" ++ chars.nbsp
                                ]
                        , br
                        ]
                    ]

        showMessageQueueCheckBox =
            span []
                [ b "Show protocol: "
                , input
                    [ type_ "checkbox"
                    , checked model.showMessageQueue
                    , onCheck SetShowMessageQueue
                    ]
                    []
                ]
    in
    div [ align "center" ]
        [ Lazy.lazy6 Board.render
            Click
            bsize
            game.player
            gameState
        , span
            []
            [ br
            , case model.error of
                Nothing ->
                    text ""

                Just err ->
                    span [ style "color" "red" ]
                        [ text err
                        , br
                        ]
            , if not isReviewingOrArchiving then
                text ""

              else
                span messageStyles
                    [ text reviewingMessage
                    , br
                    ]
            , if message == "" then
                text ""

              else
                span messageStyles
                    [ text message
                    , br
                    ]
            , if not isReviewingOrArchiving then
                text ""

              else
                movesSpan
            , if
                (gameState.winner /= NoWinner)
                    || (gameState.moves == [] && gameState.players == Types.emptyPlayerNames)
              then
                text ""

              else
                span []
                    [ b "Whose turn: "
                    , text <| localizedPlayerName gameState.whoseTurn game
                    ]
            , if corruptJumped == AskAsk || makeHulk == AskAsk then
                if isReviewingOrArchiving then
                    text ""

                else
                    div
                        [ style "border" <| "3px solid orange"
                        , style "padding" "5px"
                        , style "width" "fit-content"
                        ]
                        [ if corruptJumped == AskAsk then
                            span []
                                [ b "Corrupt Jumped Piece: "
                                , button [ onClick <| CorruptJumpedUI (AskYes ()) ]
                                    [ text "Yes" ]
                                , button [ onClick <| CorruptJumpedUI AskNo ]
                                    [ text "No" ]
                                , br
                                ]

                          else
                            text ""
                        , if makeHulk == AskAsk then
                            span []
                                [ b "Click a Golem to make it a Hulk, Click an empty square to not make a hulk."
                                , br
                                ]

                          else
                            text ""
                        ]

              else
                text ""
            , if not game.isLocal && game.isLive then
                span []
                    [ if white == "" || black == "" then
                        br

                      else
                        let
                            chatSettings =
                                model.chatSettings
                        in
                        span []
                            [ br
                            , ElmChat.styledInputBox [ id ids.chatInput ]
                                []
                                --width in chars
                                30
                                --id
                                "Send"
                                ChatSend
                                chatSettings
                            , text " "
                            , button [ onClick ChatClear ]
                                [ text "Clear" ]
                            , ElmChat.chat chatSettings
                            ]
                    , b "White: "
                    , text <|
                        case white of
                            "" ->
                                ""

                            _ ->
                                localizedPlayerName game.player game
                    , br
                    , b "Black: "
                    , text <|
                        case black of
                            "" ->
                                ""

                            _ ->
                                localizedPlayerName (Types.otherPlayer game.player) game

                    --, br
                    ]

              else
                text ""
            , if isReviewingOrArchiving then
                text ""

              else
                let
                    undoLen =
                        List.length gameState.undoStates
                in
                if undoLen == 0 then
                    text ""

                else
                    span []
                        [ br
                        , button [ onClick <| SendUndoJumps UndoOneJump ]
                            [ text "Undo Jump" ]
                        , if undoLen <= 1 then
                            text ""

                          else
                            span []
                                [ text " "
                                , button [ onClick <| SendUndoJumps UndoAllJumps ]
                                    [ text "Undo All Jumps" ]
                                ]
                        ]
            , br
            , if isReviewingOrArchiving || not game.isLive || gameState.winner /= NoWinner || inCrowd then
                text ""

              else if not yourTurn && hasBothPlayers then
                span []
                    [ b "Undo request: "
                    , input
                        [ onInput SetRequestUndoMessage
                        , value model.requestUndoMessage
                        , size 20
                        , onEnter SendRequestUndo
                        ]
                        []
                    , text " "
                    , button [ onClick SendRequestUndo ]
                        [ text "Send" ]
                    , case gameState.requestUndo of
                        NoRequestUndo ->
                            text ""

                        RequestUndo msg ->
                            span []
                                [ br
                                , b "Undo request sent: "
                                , text <| maybeNoText msg
                                ]

                        DenyUndo msg ->
                            span []
                                [ br
                                , span [ style "color" "red" ]
                                    [ text "Denied: "
                                    , text <| maybeNoText msg
                                    ]
                                ]
                    , br
                    ]

              else
                case gameState.requestUndo of
                    NoRequestUndo ->
                        text ""

                    DenyUndo msg ->
                        span []
                            [ text "Undo denial sent: "
                            , text <| maybeNoText msg
                            , br
                            ]

                    RequestUndo msg ->
                        span []
                            [ span [ style "color" "red" ]
                                [ text "Undo requested: "
                                , text <| maybeNoText msg ++ " "
                                ]
                            , button [ onClick SendAcceptUndo ]
                                [ text "Accept" ]
                            , br
                            , b "Deny undo: "
                            , input
                                [ onInput SetDenyUndoMessage
                                , value model.denyUndoMessage
                                , size 20
                                , onEnter SendDenyUndo
                                ]
                                []
                            , text " "
                            , button [ onClick SendDenyUndo ]
                                [ text "Send" ]
                            , br
                            ]
            , if isReviewingOrArchiving then
                text ""

              else
                movesSpan
            , let
                { games, whiteWins, blackWins } =
                    gameState.score

                yourWins =
                    game.yourWins

                player =
                    game.player

                otherPlayer =
                    Types.otherPlayer player
              in
              if games == 0 then
                text ""

              else
                let
                    ( yourWinString, otherWinString ) =
                        if game.isLocal then
                            ( "White won " ++ String.fromInt whiteWins
                            , "Black won " ++ String.fromInt blackWins
                            )

                        else
                            ( localizedPlayerName player game
                                ++ " won "
                                ++ String.fromInt yourWins
                            , localizedPlayerName otherPlayer game
                                ++ " won "
                                ++ String.fromInt (games - yourWins)
                            )
                in
                span []
                    [ text <|
                        String.fromInt games
                            ++ (if games == 1 then
                                    "game, "

                                else
                                    " games, "
                               )
                    , text yourWinString
                    , text ", "
                    , text otherWinString
                    , text " "
                    , br
                    ]
            , if model.showMessageQueue then
                span []
                    [ messageQueueDiv theStyle model
                    , showMessageQueueCheckBox
                    , br
                    ]

              else
                br
            , b "Rotate: "
            , radio "rotate"
                "white down"
                (model.rotate == RotateWhiteDown)
                False
                (SetRotate RotateWhiteDown)
            , text " "
            , radio "rotate"
                "player down"
                (model.rotate == RotatePlayerDown)
                False
                (SetRotate RotatePlayerDown)
            , br
            , b "Local: "
            , input
                [ type_ "checkbox"
                , checked game.isLocal
                , onCheck SetIsLocal
                , disabled <|
                    (not game.isLocal && game.isLive)
                        || showingArchiveOrMove model
                ]
                []
            , case model.notificationAvailable of
                Just True ->
                    let
                        isDisabled =
                            model.notificationPermission == Just PermissionDenied

                        theTitle =
                            if isDisabled then
                                "Notifications are disabled in the browser. You'll have to fix this in browser settings."

                            else
                                "Check to use system notifications."
                    in
                    span []
                        [ text " "
                        , b "Notifications: "
                        , input
                            [ type_ "checkbox"
                            , checked model.notificationsEnabled
                            , onCheck SetNotificationsEnabled
                            , disabled isDisabled
                            , title theTitle
                            ]
                            []
                        ]

                _ ->
                    text ""
            , text " "
            , b "Sound: "
            , input
                [ type_ "checkbox"
                , checked model.soundEnabled
                , onCheck SetSoundEnabled
                ]
                []
            , text " "
            , if isReviewingOrArchiving || inCrowd then
                text ""

              else
                button
                    [ onClick NewGame
                    , disabled (not <| isPlaying model)
                    ]
                    [ text <|
                        if gameState.winner == NoWinner then
                            "Resign"

                        else
                            "New Game"
                    ]
            , div [ align "center" ]
                [ if game.isLive then
                    div [ align "center" ]
                        [ b "Session ID: "
                        , text model.gameid
                        , br
                        , b "Archives: "
                        , archiveSelector model
                        , br
                        , button
                            [ onClick Disconnect ]
                            [ text "Disconnect" ]
                        ]

                  else
                    div [ align "center" ]
                        [ b "Your Name: "
                        , input
                            [ onInput SetName
                            , value settings.name
                            , size 20
                            ]
                            []
                        , br

                        {-
                           , b "Server: "
                           , input
                               [ onInput SetServerUrl
                               , value model.serverUrl
                               , size 40
                               , disabled True
                               ]
                               []
                           , text " "
                        -}
                        , b "Public: "
                        , input
                            [ type_ "checkbox"
                            , checked settings.isPublic
                            , onCheck SetIsPublic
                            ]
                            []
                        , if not settings.isPublic then
                            text ""

                          else
                            span []
                                [ b " for name: "
                                , input
                                    [ onInput SetForName
                                    , value settings.forName
                                    , size 20
                                    , id ids.forName
                                    ]
                                    []
                                ]
                        , text " "
                        , button
                            [ onClick StartGame
                            , disabled <| settings.name == ""
                            ]
                            [ text "Start Session" ]
                        , br
                        , b "Session ID: "
                        , input
                            [ onInput SetGameid
                            , value model.gameid
                            , size 16
                            , onEnter Join
                            ]
                            []
                        , text " "
                        , button
                            [ onClick Join
                            , disabled <|
                                (settings.name == "")
                                    || (model.gameid == "")
                            ]
                            [ text "Join"
                            ]
                        , br
                        , b "Archives: "
                        , archiveSelector model
                        ]
                ]
            ]
        , p []
            [ if model.showMessageQueue then
                text ""

              else
                showMessageQueueCheckBox
            , b " Dark mode: "
            , input
                [ type_ "checkbox"
                , checked <| model.styleType == DarkStyle
                , onCheck SetDarkMode
                ]
                []
            ]
        , footerParagraph
        , p []
            [ let
                really =
                    model.reallyClearStorage

                ( msg, label ) =
                    if really then
                        ( ClearStorage, "Clear Storage Now!" )

                    else
                        ( MaybeClearStorage, "Clear Storage!" )
              in
              button
                [ onClick msg
                , title "Clear Local Storage. Cannot be undone!"
                ]
                [ text label ]
            , text " "
            , button
                [ onClick Reload
                , title "Reload the page from the web server."
                ]
                [ text "Reload" ]
            ]
        ]


notificationStyles : Bool -> Bool -> GameState -> List (Attribute Msg)
notificationStyles yourTurn playing gameState =
    [ style "color"
        (if not yourTurn && (not playing || gameState.winner == NoWinner) then
            "green"

         else
            "orange"
        )
    , style "font-weight"
        (if gameState.winner == NoWinner then
            "normal"

         else
            "bold"
        )
    , style "font-size" <|
        if yourTurn then
            "125%"

        else
            "100%"
    ]


archiveSelector : Model -> Html Msg
archiveSelector model =
    let
        game =
            model.game

        archives =
            game.archives
    in
    if archives == [] then
        text ""

    else
        let
            currentArchive =
                case model.showArchive of
                    Nothing ->
                        Nothing

                    Just ( _, index ) ->
                        Just index
        in
        select [ onInput SetArchiveIndex ] <|
            [ option
                [ value ""
                , selected <| currentArchive == Nothing
                ]
                [ text "Chose an archived game..." ]
            ]
                ++ List.indexedMap (archiveOption model.zone currentArchive) archives


archiveOption : Zone -> Maybe Int -> Int -> ArchivedGame -> Html Msg
archiveOption zone currentIndex index { moves, players, winner } =
    let
        { white, black } =
            players

        timeString =
            case List.head <| List.reverse moves of
                Nothing ->
                    ""

                Just { time } ->
                    shortDateAndTimeString zone time

        winString =
            case winner of
                NoWinner ->
                    "No winner"

                WhiteWinner reason ->
                    "white won " ++ winReasonToDescription reason

                BlackWinner reason ->
                    "black won " ++ winReasonToDescription reason
    in
    option
        [ value <| String.fromInt index
        , selected <| Just index == currentIndex
        ]
        [ text <|
            (white ++ " vs. " ++ black)
                ++ (if timeString == "" then
                        ""

                    else
                        ", " ++ timeString
                   )
                ++ (", " ++ winString)
        ]


moveString : Model -> ( String, Int, Int )
moveString model =
    let
        gameState =
            model.game.gameState

        moves =
            gameState.moves

        len =
            4

        movesLength =
            List.length moves

        ellipsis =
            if movesLength > len then
                ", ..."

            else
                ""

        ( prefix, count, index ) =
            case model.showMove of
                Nothing ->
                    ( "", movesLength, 0 )

                Just ( fullGame, idx ) ->
                    ( if idx > 0 then
                        if movesLength == 0 then
                            "..."

                        else
                            "..., "

                      else
                        ""
                    , List.length fullGame.gameState.moves
                    , idx
                    )

        head =
            List.take 4 moves
    in
    ( prefix ++ movesToString head ++ ellipsis, count, index )


footerParagraph : Html Msg
footerParagraph =
    p []
        [ a
            [ href "#"
            , onClick <| SetPage PublicPage
            ]
            [ text "Public" ]
        , text " "
        , a
            [ href "#"
            , onClick <| SetPage InstructionsPage
            ]
            [ text "Instructions" ]
        , text " "
        , a
            [ href "#"
            , onClick <| SetPage RulesPage
            ]
            [ text "Rules" ]
        , text " "
        , a
            [ href "#"
            , onClick <| SetPage StatisticsPage
            ]
            [ text "Statistics" ]
        , br
        , a
            [ href "https://github.com/billstclair/agog/"
            , target "_blank"
            ]
            [ text "GitHub" ]
        , text " "
        , a
            [ href "https://gibgoygames.com/"
            , target "_blank"
            ]
            [ text "Gib Goy Games" ]
        , br
        , text <| chars.copyright ++ " 2019-2022 Bill St. Clair <"
        , a [ href "mailto:GibGoyGames@gmail.com" ]
            [ text "GibGoyGames@gmail.com" ]
        , text ">"
        , br
        , a
            [ href "https://github.com/billstclair/agog/blob/main/LICENSE"
            , target "_blank"
            ]
            [ text "MIT License" ]
        ]


pairup : List String -> List ( String, String )
pairup strings =
    let
        loop list res =
            case list of
                [] ->
                    List.reverse res

                [ x ] ->
                    List.reverse <| ( x, "" ) :: res

                x :: (y :: tail) ->
                    loop tail (( x, y ) :: res)
    in
    loop strings []


movesToString : List OneMove -> String
movesToString moves =
    List.map ED.oneMoveToPrettyString moves
        |> String.join ", "


radio : String -> String -> Bool -> Bool -> msg -> Html msg
radio group name isChecked isDisabled msg =
    label []
        [ input
            [ type_ "radio"
            , Attributes.name group
            , onClick msg
            , checked isChecked
            , disabled isDisabled
            ]
            []
        , text name
        ]


rulesDiv : Bool -> List (Html Msg) -> Html Msg
rulesDiv =
    Documentation.rulesDiv


playButton : Html Msg
playButton =
    Documentation.playButtonHtml <| SetPage MainPage


instructionsPage : Int -> Model -> Html Msg
instructionsPage bsize model =
    Documentation.instructions (SetPage MainPage) <| Just footerParagraph


rulesPage : Int -> Model -> Html Msg
rulesPage bsize model =
    Documentation.rules (SetPage MainPage) <| Just footerParagraph


th : String -> Html Msg
th string =
    Html.th [] [ text string ]


publicPage : Int -> Model -> Html Msg
publicPage bsize model =
    let
        game =
            model.game

        settings =
            model.settings

        name =
            settings.name

        ( waiting, inplay ) =
            discriminatePublicGames model.publicGames
    in
    rulesDiv False
        [ rulesDiv True
            [ h2 [ align "center" ]
                [ text "Public Sessions" ]
            , playButton
            , p [ align "center" ]
                [ b "Your Name: "
                , input
                    [ onInput SetName
                    , value name
                    , size 20
                    ]
                    []
                , if name /= "" then
                    text ""

                  else
                    span [ style "color" "red" ]
                        [ br
                        , text "To join a session, you must enter a name."
                        ]
                ]
            , p [ align "center" ]
                [ if game.isLive then
                    p [ style "color" "red" ]
                        [ if game.watcherName == Nothing then
                            text "You're playing a game. What are you doing here?"

                          else
                            text "You are a spectator for a live game. Disconnect or create a new session to join a new one."
                        ]

                  else
                    text ""
                ]
            , if waiting == [] then
                h3 [] [ text "There are no games waiting for an opponent." ]

              else
                span []
                    [ h3 [] [ text "Games waiting for an opponent:" ]
                    , table
                        [ class "prettytable"
                        , style "color" "black"
                        ]
                      <|
                        List.concat
                            [ [ tr []
                                    [ th "Session ID"
                                    , th "Creator"
                                    , th "Player"
                                    , th "For you"
                                    ]
                              ]
                            , List.map
                                (renderPublicGameRow name game.isLive model)
                                waiting
                            ]
                    ]
            , if inplay == [] then
                text ""

              else
                span []
                    [ h3 [] [ text <| "Games that you may observe:" ]
                    , table
                        [ class "prettytable"
                        , style "color" "black"
                        ]
                      <|
                        List.concat
                            [ [ tr []
                                    [ th "Session ID"
                                    , th "White"
                                    , th "Black"
                                    , th "Observing"
                                    , th "Started"
                                    , th "Moves"
                                    , th "Last Move"
                                    ]
                              ]
                            , renderInplayPublicGameRow model.tick
                                model.zone
                                model.gameid
                                game.isLive
                                name
                                model
                            ]
                    ]
            , playButton
            , footerParagraph
            ]
        ]


discriminatePublicGames : List PublicGameAndPlayers -> ( List PublicGameAndPlayers, List PublicGameAndPlayers )
discriminatePublicGames games =
    let
        folder game ( waiting, inplay ) =
            let
                { white, black } =
                    game.players
            in
            if white /= "" && black /= "" then
                ( waiting, game :: inplay )

            else
                ( game :: waiting, inplay )
    in
    List.foldr folder ( [], [] ) games


renderInplayPublicGameRow : Posix -> Zone -> List GameId -> Bool -> String -> Model -> PublicGameAndPlayers -> Html Msg
renderInplayPublicGameRow tick zone gameids connected name model { publicGame, players, watchers, moves, startTime, endTime } =
    let
        { gameid } =
            publicGame

        { white, black } =
            players

        center =
            style "text-align" "center"

        ( dontLink, ( whiteText, blackText, watcherText ) ) =
            if gameid /= model.gameid then
                ( connected || name == "" || name == white || name == black
                , ( text, text, text )
                )

            else
                let
                    game =
                        model.game
                in
                case game.player of
                    WhitePlayer ->
                        ( False, ( b, text, text ) )

                    BlackPlayer ->
                        ( False, ( text, b, text ) )
    in
    tr []
        [ td [ center ]
            [ if dontLink then
                text gameid

              else
                a
                    [ href "#"
                    , onClick <| JoinGameAsWatcher gameid
                    ]
                    [ text gameid ]
            ]
        , td [ center ]
            [ whiteText white ]
        , td [ center ]
            [ blackText black ]
        , td [ center ]
            [ watcherText <| String.fromInt watchers ]
        , td [ center ]
            [ if moves == 0 then
                text chars.nbsp

              else
                text <| shortDateAndTimeString zone startTime
            ]
        , td [ center ]
            [ text <| String.fromInt moves ]
        , td [ center ]
            [ if moves == 0 then
                text chars.nbsp

              else
                text <|
                    DateFormat.Relative.relativeTime
                        tick
                        (min (Time.posixToMillis endTime) (Time.posixToMillis tick)
                            |> Time.millisToPosix
                        )
            ]
        ]


renderPublicGameRow : String -> Bool -> Model -> PublicGameAndPlayers -> Html Msg
renderPublicGameRow name connected model { publicGame } =
    let
        { gameid, creator, player, forName } =
            publicGame

        isMyGame =
            gameid == model.gameid

        center =
            style "text-align" "center"
    in
    tr []
        [ td [ center ]
            [ if isMyGame || connected || name == "" then
                text gameid

              else
                a
                    [ href "#"
                    , onClick <| JoinGame gameid
                    ]
                    [ text gameid ]
            ]
        , td [ center ]
            [ if isMyGame then
                text <| "You (" ++ creator ++ ")"

              else
                text creator
            ]
        , td [ center ] [ text <| playerString player ]
        , td [ center ]
            [ if isMyGame then
                text <| Maybe.withDefault "" forName

              else
                input
                    [ type_ "checkbox"
                    , checked <| name /= "" && Interface.forNameMatches name forName
                    , disabled True
                    ]
                    []
            ]
        ]


alignCenterStyle : Html.Attribute msg
alignCenterStyle =
    style "text-align" "center"


alignRightStyle : Html.Attribute msg
alignRightStyle =
    style "text-align" "right"


smallTextStyle : Html.Attribute msg
smallTextStyle =
    style "font-size" "80%"


type alias Format =
    List DateFormat.Token


dateAndTimeFormat : Format
dateAndTimeFormat =
    [ DateFormat.dayOfMonthNumber
    , DateFormat.text " "
    , DateFormat.monthNameFull
    , DateFormat.text " "
    , DateFormat.yearNumber
    , DateFormat.text ", "
    , DateFormat.hourNumber
    , DateFormat.text ":"
    , DateFormat.minuteFixed
    , DateFormat.amPmLowercase
    ]


shortDateAndTimeFormat : Format
shortDateAndTimeFormat =
    [ DateFormat.monthNumber
    , DateFormat.text "/"
    , DateFormat.dayOfMonthNumber
    , DateFormat.text "/"
    , DateFormat.yearNumberLastTwo
    , DateFormat.text " "
    , DateFormat.hourNumber
    , DateFormat.text ":"
    , DateFormat.minuteFixed
    , DateFormat.amPmLowercase
    ]


formatPosix : Zone -> Format -> Posix -> String
formatPosix zone format posix =
    DateFormat.format format zone posix


formatUtc : Format -> Posix -> String
formatUtc format posix =
    formatPosix Time.utc format posix


dateAndTimeString : Zone -> Posix -> String
dateAndTimeString zone posix =
    formatPosix zone dateAndTimeFormat posix


shortDateAndTimeString : Zone -> Posix -> String
shortDateAndTimeString zone posix =
    formatPosix zone shortDateAndTimeFormat posix


sFormat : Format
sFormat =
    [ DateFormat.text ":"
    , DateFormat.secondFixed
    ]


msFormat : Format
msFormat =
    [ DateFormat.minuteFixed
    , DateFormat.text ":"
    , DateFormat.secondFixed
    ]


hmsFormat : Format
hmsFormat =
    [ DateFormat.hourFixed
    , DateFormat.text ":"
    , DateFormat.minuteFixed
    , DateFormat.text ":"
    , DateFormat.secondFixed
    ]


militaryFormat : Format
militaryFormat =
    [ DateFormat.hourMilitaryFixed
    , DateFormat.text ":"
    , DateFormat.minuteFixed
    , DateFormat.text ":"
    , DateFormat.secondFixed
    ]


hmsString : Bool -> Posix -> String
hmsString isMilitary posix =
    let
        millis =
            Time.posixToMillis posix

        days =
            millis // (24 * 60 * 60 * 1000)

        secs =
            (toFloat millis / 1000 |> round) - (days * 24 * 60 * 60)

        format =
            if isMilitary then
                militaryFormat

            else if days > 0 then
                hmsFormat

            else if secs < 60 then
                sFormat

            else if secs < 60 * 60 then
                msFormat

            else
                hmsFormat

        dayString =
            if days > 0 then
                String.fromInt days ++ ":"

            else
                ""
    in
    dayString
        ++ (formatUtc format <| Time.millisToPosix (1000 * secs))


roundPosix : Posix -> Posix
roundPosix posix =
    Time.posixToMillis posix
        |> (\m -> 1000 * round (toFloat m / 1000))
        |> Time.millisToPosix


movesPage : Int -> Model -> Html Msg
movesPage bsize model =
    let
        settings =
            model.settings

        game =
            model.game

        gameState =
            game.gameState

        moves =
            List.reverse gameState.moves

        realGame =
            case model.showArchive of
                Just ( g, _ ) ->
                    g

                Nothing ->
                    case model.showMove of
                        Nothing ->
                            game

                        Just ( g, _ ) ->
                            g

        realMoves =
            List.reverse realGame.gameState.moves

        gameTime =
            case List.head realMoves of
                Just move ->
                    Just move.time

                Nothing ->
                    Nothing

        ( totalTime, offsetMoves ) =
            case gameTime of
                Nothing ->
                    ( Types.posixZero, moves )

                Just time ->
                    let
                        millis =
                            Time.posixToMillis (roundPosix time)

                        moves2 =
                            moves
                                |> List.map
                                    (\move ->
                                        { move
                                            | time =
                                                (Time.posixToMillis (roundPosix move.time) - millis)
                                                    |> Time.millisToPosix
                                        }
                                    )

                        moves3 =
                            moves2
                                |> List.foldl
                                    (\move ( res, last ) ->
                                        let
                                            atime =
                                                move.time |> Time.posixToMillis
                                        in
                                        ( { move | time = Time.millisToPosix (atime - last) } :: res
                                        , atime
                                        )
                                    )
                                    ( [], 0 )
                                |> Tuple.first
                                |> List.reverse
                    in
                    ( case List.head (List.reverse moves2) of
                        Just p ->
                            p.time
                                |> Time.posixToMillis
                                |> (\a ->
                                        1000
                                            * round (toFloat a / 1000)
                                            |> Time.millisToPosix
                                   )

                        Nothing ->
                            Types.posixZero
                    , moves3
                    )

        { white, black } =
            gameState.players

        pairList : List a -> List (List a)
        pairList list =
            let
                mapper a ( evenp, res ) =
                    if evenp then
                        ( False, [ a ] :: res )

                    else
                        case res of
                            [ a1 ] :: rest ->
                                ( True, [ a1, a ] :: rest )

                            _ ->
                                -- Can't happen
                                ( True, res )
            in
            List.foldl mapper ( True, [] ) list
                |> Tuple.second
                |> List.reverse

        gameTimeSpan =
            case gameTime of
                Nothing ->
                    text ""

                Just time ->
                    span []
                        [ text <| dateAndTimeString model.zone time
                        , br
                        , b "Total time: "
                        , text <| hmsString True totalTime
                        ]

        ( winner, reason ) =
            case gameState.winner of
                NoWinner ->
                    ( Nothing, WinByCapture )

                WhiteWinner reas ->
                    ( Just WhitePlayer, reas )

                BlackWinner reas ->
                    ( Just BlackPlayer, reas )

        winString =
            case winner of
                Nothing ->
                    "No winner, yet"

                Just player ->
                    let
                        winnerName =
                            if game.isLocal then
                                case player of
                                    WhitePlayer ->
                                        "White"

                                    BlackPlayer ->
                                        "Black"

                            else
                                case player of
                                    WhitePlayer ->
                                        white

                                    BlackPlayer ->
                                        black

                        reasonString =
                            case reason of
                                WinByCapture ->
                                    "capture"

                                WinBySanctum ->
                                    "sanctum"

                                WinByImmobilization ->
                                    "immobilization"

                                WinByResignation ->
                                    "resignation"

                        moveCnt =
                            (List.length moves + 1) // 2 |> String.fromInt
                    in
                    winnerName ++ " won by " ++ reasonString ++ " in " ++ moveCnt

        ( endReview, totalMoveCnt ) =
            case model.showMove of
                Nothing ->
                    ( text "", List.length moves )

                Just ( g, _ ) ->
                    ( span []
                        [ br
                        , button [ onClick <| SetMoveIndex 0 ]
                            [ text "End review" ]
                        ]
                    , List.length g.gameState.moves
                    )

        game =
            case model.showArchive of
                Just ( g, _ ) ->
                    g

                Nothing ->
                    case model.showMove of
                        Nothing ->
                            model.game

                        Just ( g, _ ) ->
                            g

        inCrowd =
            game.watcherName /= Nothing
    in
    rulesDiv False
        [ rulesDiv True
            [ h2 [ align "center" ]
                [ text "Moves" ]
            , p [] [ playButton ]
            , if game.isLocal then
                p []
                    [ gameTimeSpan
                    , br
                    , text winString
                    , endReview
                    ]

              else
                p []
                    [ gameTimeSpan
                    , br
                    , b "White: "
                    , text white
                    , text ", "
                    , b "Black: "
                    , text black
                    , br
                    , text winString
                    , endReview
                    , if not game.isLive || game.gameState.winner /= NoWinner then
                        text ""

                      else
                        let
                            whoseTurn =
                                game.gameState.whoseTurn

                            yourTurn =
                                not inCrowd && whoseTurn == game.player
                        in
                        span (notificationStyles yourTurn True game.gameState)
                            [ if model.showArchive == Nothing then
                                text ""

                              else
                                span []
                                    [ br
                                    , text "This is an archived game."
                                    ]
                            , br
                            , if inCrowd then
                                span []
                                    [ text <| chars.watchingMessage ++ "."
                                    , br
                                    , text "It's "
                                    , text <| playerName whoseTurn game
                                    , text "'s turn to move."
                                    ]

                              else if yourTurn then
                                text "It's your turn in the game."

                              else
                                let
                                    players =
                                        game.gameState.players

                                    name =
                                        if game.player == WhitePlayer then
                                            players.black

                                        else
                                            players.white
                                in
                                text <| "Waiting for " ++ name ++ " to move"
                            ]
                    ]
            , text "Click in the table see the board after that move"
            , br
            , table
                [ class "prettytable"
                , style "color" "black"
                ]
              <|
                [ tr []
                    [ th chars.nbsp
                    , th chars.nbsp
                    , th "White"
                    , th "Black"
                    , th chars.nbsp
                    ]
                ]
                    ++ List.indexedMap (movesRow totalMoveCnt)
                        (offsetMoves |> pairList)
            , p [] [ playButton ]
            , footerParagraph
            ]
        ]


isEven : Int -> Bool
isEven x =
    x == x // 2 * 2


movesRow : Int -> Int -> List OneMove -> Html Msg
movesRow moveCnt index moves =
    let
        ( maybeWhite, maybeBlack ) =
            case List.take 2 moves of
                [ white ] ->
                    ( Just white, Nothing )

                [ white, black ] ->
                    ( Just white, Just black )

                _ ->
                    ( Nothing, Nothing )

        beforeWhiteIdx =
            moveCnt - 2 * index

        whiteIdx =
            if maybeWhite == Nothing then
                beforeWhiteIdx

            else
                beforeWhiteIdx - 1

        blackIdx =
            if maybeBlack == Nothing then
                whiteIdx

            else
                whiteIdx - 1
    in
    tr []
        [ td
            [ alignCenterStyle
            , onClick <| ReviewMoves beforeWhiteIdx
            ]
            [ text (String.fromInt <| index + 1) ]
        , td
            [ alignRightStyle
            , smallTextStyle
            , onClick <| ReviewMoves beforeWhiteIdx
            ]
            [ case maybeWhite of
                Nothing ->
                    text chars.nbsp

                Just { time } ->
                    if index == 0 && time == Types.posixZero then
                        text chars.nbsp

                    else
                        text <| hmsString False time
            ]
        , td
            [ alignCenterStyle
            , onClick <| ReviewMoves whiteIdx
            ]
            [ case maybeWhite of
                Nothing ->
                    text chars.nbsp

                Just white ->
                    text <| ED.oneMoveToPrettyString white
            ]
        , td
            [ alignCenterStyle
            , onClick <| ReviewMoves blackIdx
            ]
            [ case maybeBlack of
                Nothing ->
                    text chars.nbsp

                Just black ->
                    text <| ED.oneMoveToPrettyString black
            ]
        , td
            [ alignRightStyle
            , smallTextStyle
            , onClick <| ReviewMoves blackIdx
            ]
            [ case maybeBlack of
                Nothing ->
                    text chars.nbsp

                Just { time } ->
                    text <| hmsString False time
            ]
        ]


playerString : Player -> String
playerString player =
    case player of
        WhitePlayer ->
            "White"

        BlackPlayer ->
            "Black"


statisticsPage : Int -> Model -> Html Msg
statisticsPage bsize model =
    let
        game =
            model.game
    in
    rulesDiv False
        [ rulesDiv True
            [ h2 [ align "center" ]
                [ text "Statistics" ]
            , p [] [ playButton ]
            , if game.isLocal then
                p [] [ text "There are no live updates in local mode." ]

              else
                text ""
            , case model.statistics of
                Nothing ->
                    p []
                        [ text "There are no statistics." ]

                Just statistics ->
                    let
                        ( startTime, updateTime ) =
                            model.statisticsTimes

                        uptime =
                            case startTime of
                                Nothing ->
                                    0

                                Just time ->
                                    Time.posixToMillis model.tick - time
                    in
                    span []
                        [ table
                            [ class "prettytable"
                            , style "color" "black"
                            ]
                          <|
                            [ tr []
                                [ th "Statistic"
                                , th "Count"
                                ]
                            ]
                                ++ List.map (statisticsRow statistics) Types.statisticsKeyOrder
                        , case Dict.get statisticsKeys.finishedGames statistics of
                            Nothing ->
                                text ""

                            Just finishedGames ->
                                span []
                                    [ case Dict.get statisticsKeys.whiteWon statistics of
                                        Nothing ->
                                            text ""

                                        Just whiteWon ->
                                            span []
                                                [ b "White wins: "
                                                , text <| String.fromInt (whiteWon * 100 // finishedGames)
                                                , text "%"
                                                , br
                                                ]
                                    , case Dict.get statisticsKeys.totalMoves statistics of
                                        Nothing ->
                                            text ""

                                        Just totalMoves ->
                                            span []
                                                [ b "Average moves/game: "
                                                , text <| String.fromInt (totalMoves // finishedGames)
                                                , br
                                                ]
                                    ]
                        , case updateTime of
                            Nothing ->
                                text ""

                            Just time ->
                                span []
                                    [ b "Last update time: "
                                    , text <|
                                        DateFormat.Relative.relativeTime
                                            model.tick
                                            (Time.millisToPosix <|
                                                min time
                                                    (Time.posixToMillis model.tick)
                                            )
                                    , br
                                    ]
                        , case startTime of
                            Nothing ->
                                text ""

                            Just time ->
                                span []
                                    [ b "Server uptime: "
                                    , text <|
                                        uptimeString model.tick
                                            (Time.millisToPosix time)
                                    , br
                                    ]
                        ]
            , p [] [ playButton ]
            , footerParagraph
            ]
        ]


monthNumber : Time.Month -> Int
monthNumber month =
    let
        months =
            [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
    in
    case LE.elemIndex month months of
        Nothing ->
            0

        Just idx ->
            idx


uptimeString : Posix -> Posix -> String
uptimeString now start =
    let
        delta =
            (Time.posixToMillis now - Time.posixToMillis start) // 1000

        days =
            delta // (3600 * 24)

        deltaMinusDays =
            delta - days * 3600 * 24

        hours =
            deltaMinusDays // 3600

        deltaMinusHours =
            deltaMinusDays - hours * 3600

        minutes =
            deltaMinusHours // 60

        showDays =
            days /= 0

        showHours =
            showDays || hours /= 0

        dayString =
            if showDays then
                String.fromInt days ++ plural days " day, " " days, "

            else
                ""

        hourString =
            if showHours then
                String.fromInt hours ++ plural hours " hour, " " hours, "

            else
                ""

        minuteString =
            String.fromInt minutes ++ plural minutes " minute" " minutes"

        plural num one notone =
            if num == 1 then
                one

            else
                notone
    in
    dayString ++ hourString ++ minuteString


statisticsRow : Statistics -> (StatisticsKeys -> String) -> Html Msg
statisticsRow statistics keystring =
    let
        property =
            keystring Types.statisticsKeys
    in
    case Dict.get property statistics of
        Nothing ->
            text ""

        Just value ->
            tr []
                [ td [] [ text property ]
                , td [] [ text <| String.fromInt value ]
                ]


b : String -> Html msg
b s =
    Html.b [] [ text s ]


codestr : Int -> String
codestr code =
    String.fromList [ Char.fromCode code ]


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    , mdash = codestr 0x2014
    , watchingMessage = "You are a spectator for this game, not playing"
    }


toMdashes : String -> String
toMdashes string =
    String.replace "--" chars.mdash string
