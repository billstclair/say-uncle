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
import Cards exposing (Card)
import Deck exposing (Deck, ShuffledDeck)
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as LE
import Random exposing (Seed)
import SayUncle.Types as Types
    exposing
        ( Board
        )
import Set exposing (Set)
import Svg
    exposing
        ( Attribute
        , Svg
        , defs
        , foreignObject
        , g
        , line
        , marker
        , path
        , rect
        , svg
        )
import Svg.Attributes as Attributes
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

        ( stock, tableauList ) =
            dealLoop tableauCount deck4 []

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


render : msg -> Board -> Html msg
render clickWrapper board =
    Html.text ""


fontSize : Int -> Int
fontSize delta =
    delta // 2


fontStyle : Int -> String
fontStyle fsize =
    "font-size: "
        ++ tos fsize
        ++ ";"
