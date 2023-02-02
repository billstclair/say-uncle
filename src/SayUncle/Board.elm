--------------------------------------------------------------------
--
-- Board.elm
-- Say Uncle board, storage and rendering.
-- Copyright (c) 2019-2023 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module SayUncle.Board exposing
    ( initial
    , render
    )

import Array exposing (Array)
import Cards exposing (Card(..), Face(..), Suit(..))
import CardsView exposing (CardDescription, Size)
import Deck exposing (Deck, ShuffledDeck)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import List.Extra as LE
import Random exposing (Seed)
import SayUncle.Types as Types
    exposing
        ( Board
        , BoardClick(..)
        , GameState
        , Player
        , PlayerNames
        , Size
        )
import Set exposing (Set)
import Svg
    exposing
        ( Attribute
        , Svg
        )
import Svg.Attributes as Svga
    exposing
        ( cx
        , cy
        , d
        , fill
        , fillOpacity
        , fontSize
        , height
        , markerEnd
        , markerHeight
        , markerStart
        , markerWidth
        , orient
        , points
        , r
        , refX
        , refY
        , rotate
        , rx
        , ry
        , stroke
        , strokeDasharray
        , strokeOpacity
        , strokeWidth
        , textAnchor
        , transform
        , viewBox
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )
import Svg.Events as Events


mergeDecks : ShuffledDeck -> ShuffledDeck -> ShuffledDeck
mergeDecks deck1 deck2 =
    let
        cards =
            Deck.getCards deck2

        folder : Card -> ShuffledDeck -> ShuffledDeck
        folder card deck =
            Deck.appendCard card deck
    in
    List.foldl folder deck1 <| Deck.getCards deck2


initial : Int -> Seed -> ( Board, Seed )
initial playerCount seed =
    let
        deckCount =
            ((playerCount - 1) // 4) + 1

        deckLoop : Int -> ( ShuffledDeck, Seed ) -> ( ShuffledDeck, Seed )
        deckLoop cnt ( deck2, seed3 ) =
            if cnt <= 0 then
                ( deck2, seed3 )

            else
                let
                    ( newDeck, seed4 ) =
                        Random.step Deck.randomDeck seed3
                in
                deckLoop (cnt - 1) ( mergeDecks deck2 newDeck, seed4 )

        ( deck, seed2 ) =
            deckLoop (deckCount - 1) (Random.step Deck.randomDeck seed)

        dealLoop : Int -> ShuffledDeck -> List Card -> ( ShuffledDeck, List Card )
        dealLoop cardsLeft deck2 cards =
            if cardsLeft <= 0 then
                ( deck2, List.reverse cards )

            else
                let
                    ( card, deck3 ) =
                        Deck.draw deck2
                in
                dealLoop (cardsLeft - 1) deck3 <| card :: cards

        handsLoop : Int -> ShuffledDeck -> List (List Card) -> ( ShuffledDeck, Array (List Card) )
        handsLoop handIdx deck2 cardsList =
            if handIdx <= 0 then
                ( deck2, Array.fromList <| List.reverse cardsList )

            else
                let
                    ( deck3, cards ) =
                        dealLoop 5 deck2 []
                in
                handsLoop (handIdx - 1) deck3 <| cards :: cardsList

        ( deck4, hands ) =
            handsLoop playerCount deck []

        tableauCount =
            playerCount * 5

        ( rawStock, tableauList ) =
            dealLoop tableauCount deck4 []

        rawStockSize =
            Deck.length rawStock

        stockSize =
            (rawStockSize // playerCount) * playerCount

        discards =
            Debug.log "discards" <|
                rawStockSize
                    - stockSize

        discardLoop : Int -> ShuffledDeck -> ShuffledDeck
        discardLoop cnt deck2 =
            if cnt <= 0 then
                deck2

            else
                let
                    ( _, deck3 ) =
                        Deck.draw deck2
                in
                discardLoop (cnt - 1) deck3

        stock =
            discardLoop discards rawStock

        tableau =
            Array.fromList <| List.map Just tableauList
    in
    ( { tableau = tableau
      , stock = stock
      , turnedStock = Nothing
      , hands = hands
      }
    , seed2
    )



---
--- Rendering
---


tos : Int -> String
tos x =
    String.fromInt x


lineWidthO2 : Int
lineWidthO2 =
    1


lineWidth : Int
lineWidth =
    lineWidthO2 * 2


isTableauEmpty : Array (Maybe Card) -> Bool
isTableauEmpty tableau =
    let
        loop : Int -> Bool
        loop idx =
            if idx < 0 then
                True

            else
                case Array.get idx tableau of
                    Nothing ->
                        loop (idx - 1)

                    Just _ ->
                        False
    in
    loop <| Array.length tableau - 1


emptyTableau : Array (Maybe Card)
emptyTableau =
    Array.empty


br : Html msg
br =
    Html.br [] []


render : (BoardClick -> msg) -> Size -> GameState -> Html msg
render wrapper windowSize { board, players, whoseTurn } =
    let
        { tableau, stock, turnedStock, hands } =
            board
    in
    div [] <|
        if isTableauEmpty tableau then
            let
                stockHtml =
                    renderStock wrapper windowSize stock turnedStock
            in
            [ stockHtml
            , br
            , renderPlayerHand wrapper windowSize whoseTurn players hands
            ]

        else
            let
                tableauHtml =
                    renderTableau wrapper windowSize tableau
            in
            [ tableauHtml
            , br
            , renderPlayerHand wrapper windowSize whoseTurn players hands
            ]


renderTableau : (BoardClick -> msg) -> Size -> Array (Maybe Card) -> Html msg
renderTableau wrapper windowSize tableau =
    let
        width =
            (toFloat (windowSize.width - 5) / 10) - 5 |> floor

        height =
            toFloat width / cardWidthOverHeight |> floor

        loop : Int -> ( Int, Int ) -> List (Svg msg) -> List (Svg msg)
        loop cnt ( x, y ) res =
            if cnt < 0 then
                res

            else
                let
                    nextx =
                        x + width + 5

                    nexty =
                        if modBy (cnt + 1) 10 == 0 then
                            y + height + 5

                        else
                            y
                in
                case Array.get cnt tableau of
                    Just (Just card) ->
                        let
                            description =
                                CardsView.cardToSvg card height

                            cardSvg =
                                Svg.g
                                    [ Svga.transform <|
                                        "translate("
                                            ++ String.fromInt x
                                            ++ " "
                                            ++ String.fromInt y
                                            ++ ")"
                                    ]
                                    [ description.svg ]
                        in
                        loop (cnt - 1) ( nextx, nexty ) <| cardSvg :: res

                    _ ->
                        loop (cnt - 1) ( nextx, nexty ) res

        svgs =
            loop (Array.length tableau - 1) ( 5, 5 ) []

        totalHeight =
            (toFloat (Array.length tableau) / 10)
                |> ceiling
    in
    Svg.svg
        [ Svga.width <| String.fromInt windowSize.width
        , Svga.height <| String.fromInt totalHeight
        ]
    <|
        svgs


renderStock : (BoardClick -> msg) -> Size -> ShuffledDeck -> Maybe Card -> Html msg
renderStock wrapper windowSize stock turnedStock =
    text ""


cardWidthOverHeight : Float
cardWidthOverHeight =
    let
        aceOfHearts =
            Card Hearts Ace

        { size } =
            CardsView.cardToSvg aceOfHearts 500

        { width, height } =
            size
    in
    toFloat width / toFloat height


renderPlayerHand : (BoardClick -> msg) -> Size -> Player -> PlayerNames -> Array (List Card) -> Html msg
renderPlayerHand wrapper windowSize whoseTurn players hands =
    let
        playerName =
            case Dict.get whoseTurn players of
                Nothing ->
                    "Player " ++ String.fromInt whoseTurn

                Just name ->
                    name
    in
    text ""


fontSize : Int -> Int
fontSize delta =
    delta // 2


fontStyle : Int -> String
fontStyle fsize =
    "font-size: "
        ++ tos fsize
        ++ ";"
