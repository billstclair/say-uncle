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
    ( empty
    , getPlayerName
    , initial
    , isTableauEmpty
    , render
    , renderPlayerHand
    , sortCards
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
import Svg.Events as Svge


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


empty : Int -> ( Board, Seed )
empty playerCount =
    initial playerCount (Random.initialSeed 0)


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
                ( deck2, sortCards cards )

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
            rawStockSize - stockSize

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
    Array.toList tableau
        |> List.filterMap identity
        |> (==) []


emptyTableau : Array (Maybe Card)
emptyTableau =
    Array.empty


br : Html msg
br =
    Html.br [] []


render : (BoardClick -> msg) -> Size -> Player -> GameState -> Html msg
render wrapper windowSize player { board, players } =
    let
        { tableau, stock, turnedStock, hands } =
            board
    in
    div [] <|
        if isTableauEmpty tableau then
            [ renderStock wrapper windowSize stock turnedStock
            , br
            , renderPlayerHand wrapper windowSize player players hands
            ]

        else
            [ renderTableau wrapper windowSize tableau
            , br
            , renderPlayerHand wrapper windowSize player players hands
            ]


type alias CardsPerRow =
    { cardsPerRow : Int
    , marginx : Int
    , cardWidth : Int
    , cardHeight : Int
    }


getCardsPerRow : Size -> CardsPerRow
getCardsPerRow { width, height } =
    let
        ( cardsPerRow, marginx ) =
            if width * 4 < height * 4 then
                ( 5, width // 6 )

            else
                ( 10, 5 )

        windowWidth =
            width - 2 * marginx

        cardWidth =
            (toFloat windowWidth / toFloat cardsPerRow) - 5 |> floor

        cardHeight =
            toFloat cardWidth * cardHeightOverWidth |> floor
    in
    { cardsPerRow = cardsPerRow
    , marginx = marginx
    , cardWidth = cardWidth
    , cardHeight = cardHeight
    }


renderTableau : (BoardClick -> msg) -> Size -> Array (Maybe Card) -> Html msg
renderTableau wrapper windowSize tableau =
    let
        { cardsPerRow, marginx, cardWidth, cardHeight } =
            getCardsPerRow windowSize

        loop : Int -> ( Int, Int ) -> List (Svg msg) -> List (Svg msg)
        loop cnt ( x, y ) res =
            if cnt < 0 then
                res

            else
                let
                    ( nextx, nexty ) =
                        if modBy cardsPerRow cnt == 0 then
                            ( marginx, y + cardHeight + 5 )

                        else
                            ( x + cardWidth + 5, y )
                in
                case Array.get cnt tableau of
                    Just (Just card) ->
                        let
                            description =
                                CardsView.cardToClickableSvg
                                    (wrapper <| TableauClick card)
                                    card
                                    cardHeight

                            cardSvg =
                                Svg.g
                                    [ Svga.transform <|
                                        "translate("
                                            ++ tos x
                                            ++ " "
                                            ++ tos y
                                            ++ ")"
                                    ]
                                    [ description.svg ]
                        in
                        loop (cnt - 1) ( nextx, nexty ) <| cardSvg :: res

                    _ ->
                        loop (cnt - 1) ( nextx, nexty ) res

        svgs =
            loop (Array.length tableau - 1) ( marginx, 5 ) []

        totalHeight =
            (cardHeight
                * ((toFloat (Array.length tableau) / toFloat cardsPerRow)
                    |> ceiling
                  )
            )
                + 10
    in
    Svg.svg
        [ Svga.width <| tos windowSize.width
        , Svga.height <| tos totalHeight
        ]
    <|
        svgs


cardSize : Size
cardSize =
    let
        aceOfHearts =
            Card Hearts Ace

        { size } =
            CardsView.cardToSvg aceOfHearts 500
    in
    size


cardHeightOverWidth : Float
cardHeightOverWidth =
    toFloat cardSize.height / toFloat cardSize.width


renderStock : (BoardClick -> msg) -> Size -> ShuffledDeck -> Maybe Card -> Html msg
renderStock wrapper windowSize stock turnedStock =
    let
        s =
            Debug.log "renderStock" ( turnedStock, stock )

        cardWidth =
            windowSize.width // 6

        cardHeight =
            toFloat cardWidth * cardHeightOverWidth |> ceiling

        initialX =
            windowSize.width // 3

        turnedSvg =
            case turnedStock of
                Just card ->
                    let
                        { svg } =
                            CardsView.cardToClickableSvg
                                (wrapper <| TurnedStockClick card)
                                card
                                cardHeight
                    in
                    svg

                Nothing ->
                    emptySvg Nothing

        emptySvg maybeClick =
            Svg.rect
                (List.concat
                    [ [ Svga.width (tos <| cardWidth - 2)
                      , Svga.height (tos <| cardHeight - 2)
                      , Svga.x "1"
                      , Svga.y "1"
                      , Svga.ry "15"
                      , Svga.fill "white"
                      , Svga.stroke "black"
                      ]
                    , case maybeClick of
                        Nothing ->
                            []

                        Just click ->
                            [ Svge.onClick <| wrapper click ]
                    ]
                )
                []

        stockSvg =
            if Deck.length stock == 0 then
                emptySvg <| Just StockClick

            else
                let
                    { svg } =
                        CardsView.cardToClickableSvg
                            (wrapper StockClick)
                            Back
                            cardHeight
                in
                svg
    in
    Svg.svg
        [ Svga.width <| tos (initialX + (2 * cardWidth + 10))
        , Svga.height <| tos (cardHeight + 10)
        ]
        [ Svg.g
            [ Svga.transform <|
                "translate("
                    ++ tos initialX
                    ++ " 0)"
            ]
            [ stockSvg ]
        , Svg.g
            [ Svga.transform <|
                "translate("
                    ++ tos (initialX + cardWidth + 5)
                    ++ " 0)"
            ]
            [ turnedSvg ]
        ]


renderPlayerHand : (BoardClick -> msg) -> Size -> Player -> PlayerNames -> Array (List Card) -> Html msg
renderPlayerHand wrapper windowSize player players hands =
    case Array.get player hands of
        Nothing ->
            text ""

        Just cards ->
            let
                fractionShown =
                    0.36

                { cardWidth, cardHeight } =
                    getCardsPerRow windowSize

                deltaWidth =
                    toFloat cardWidth * fractionShown |> ceiling

                startX =
                    (windowSize.width - (9 * deltaWidth + cardWidth)) // 2

                loop : Int -> List Card -> List (Svg msg) -> List (Svg msg)
                loop x cardsTail svgs =
                    case cardsTail of
                        [] ->
                            List.reverse svgs

                        card :: rest ->
                            let
                                { svg } =
                                    CardsView.cardToClickableSvg
                                        (wrapper <| HandClick card)
                                        card
                                        cardHeight
                            in
                            loop (x + deltaWidth)
                                rest
                            <|
                                Svg.g
                                    [ Svga.transform <|
                                        "translate("
                                            ++ tos x
                                            ++ " 5)"
                                    ]
                                    [ svg ]
                                    :: svgs
            in
            Svg.svg
                [ Svga.width <| tos windowSize.width
                , Svga.height <| tos (cardHeight + 10)
                ]
            <|
                loop startX cards []


getPlayerName : Player -> PlayerNames -> String
getPlayerName player playerNames =
    case Dict.get player playerNames of
        Nothing ->
            "Player " ++ tos player

        Just name ->
            name


suitIdx : Suit -> Int
suitIdx suit =
    case suit of
        Clubs ->
            0

        Diamonds ->
            1

        Hearts ->
            2

        Spades ->
            3


suitOrder : Suit -> Suit -> Order
suitOrder suit1 suit2 =
    let
        idx1 =
            suitIdx suit1

        idx2 =
            suitIdx suit2
    in
    if idx1 < idx2 then
        LT

    else if idx1 == idx2 then
        EQ

    else
        GT


faceOrder : Face -> Face -> Order
faceOrder face1 face2 =
    let
        default1 =
            Cards.defaultFace face1

        default2 =
            Cards.defaultFace face2
    in
    if default1 < default2 then
        LT

    else if default1 == default2 then
        EQ

    else
        GT


cardOrder : Card -> Card -> Order
cardOrder card1 card2 =
    case card1 of
        Back ->
            case card2 of
                Back ->
                    EQ

                _ ->
                    LT

        Card suit1 face1 ->
            case card2 of
                Back ->
                    GT

                Card suit2 face2 ->
                    case suitOrder suit1 suit2 of
                        EQ ->
                            faceOrder face1 face2

                        order ->
                            order


sortCards : List Card -> List Card
sortCards cards =
    List.sortWith cardOrder cards
        |> sortAces


sortAces : List Card -> List Card
sortAces cards =
    let
        sortAce ace suit cs =
            let
                king =
                    Card suit King
            in
            if List.member ace cs then
                if List.member king cs then
                    if
                        List.member (Card suit Queen) cs
                            || not
                                (List.member (Card suit Two) cs
                                    && List.member (Card suit Three) cs
                                )
                    then
                        moveAfter ace king cs

                    else
                        cs

                else
                    cs

            else
                cs

        moveAfter card after cs =
            let
                loop tail head =
                    case tail of
                        [] ->
                            List.reverse head

                        c :: rest ->
                            if c == card then
                                loop rest head

                            else if c == after then
                                List.concat
                                    [ List.reverse <| card :: c :: head
                                    , rest
                                    ]

                            else
                                loop rest <| c :: head
            in
            loop cards []
    in
    sortAce (Card Spades Ace) Spades cards
        |> sortAce (Card Hearts Ace) Hearts
        |> sortAce (Card Diamonds Ace) Diamonds
        |> sortAce (Card Clubs Ace) Clubs


fontSize : Int -> Int
fontSize delta =
    delta // 2


fontStyle : Int -> String
fontStyle fsize =
    "font-size: "
        ++ tos fsize
        ++ ";"
