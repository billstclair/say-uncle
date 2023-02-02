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
    ( areMovesAJump
    , clear
    , colToString
    , computeJumperLocations
    , computeWinner
    , count
    , countColor
    , empty
    , findSquareSatisfying
    , get
    , illegalRowCol
    , initial
    , isRowColLegal
    , isUniqueMoveTo
    , mapWholeBoard
    , mapWholeBoardWithExit
    , populateLegalMoves
    , render
    , rowColToString
    , rowToString
    , set
    , stringToCol
    , stringToRow
    , stringToRowCol
    )

import Agog.Types as Types
    exposing
        ( Board
        , Color(..)
        , GameState
        , HulkAfterJump(..)
        , JumpSequence
        , MovesOrJumps(..)
        , OneCorruptibleJump
        , OneJump
        , OneMove
        , OneMoveSequence(..)
        , Piece
        , PieceType(..)
        , Player(..)
        , RowCol
        , SavedModel
        , Style
        , WinReason(..)
        , Winner(..)
        )
import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as LE
import Random exposing (Seed)
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


empty : Board
empty =
    Array.repeat 8 (Array.repeat 8 Types.emptyPiece)


initial : Board
initial =
    -- TODO
    foo



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


render : Style -> Int -> (( Int, Int ) -> msg) -> Maybe Player -> GameState -> Html msg
render style size tagger player gameState =
    let
        board =
            gameState.board

        whiteSpace =
            10

        innerSize =
            size - (2 * whiteSpace)

        sizeS =
            tos size

        delta =
            round (toFloat (innerSize - lineWidth) / 8 / sqrt 2)

        center =
            4 * delta + fontSize delta

        translate =
            round (toFloat center * (sqrt 2 - 1) / 2) + whiteSpace
    in
    svg
        [ width sizeS
        , height sizeS
        ]
        [ g
            [ transform
                ("translate("
                    ++ tos (whiteSpace + translate)
                    ++ " "
                    ++ tos (whiteSpace + translate)
                    ++ ")"
                )
            ]
            [-- TODO
            ]
        ]


fontSize : Int -> Int
fontSize delta =
    delta // 2


fontStyle : Int -> String
fontStyle fsize =
    "font-size: "
        ++ tos fsize
        ++ ";"
