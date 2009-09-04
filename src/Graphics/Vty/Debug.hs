-- Copyright 2009 Corey O'Connor
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Vty.Debug ( module Graphics.Vty.Debug
                          )
where

import Graphics.Vty.Attributes
import Graphics.Vty.Image
import Graphics.Vty.Image.Debug
import Graphics.Vty.Picture
import Graphics.Vty.Span
import Graphics.Vty.WinRegion

import Control.Monad
import Data.Array
import Data.Word

instance Show SpanOpSequence where
    show (SpanOpSequence _ row_ops)
        = concat $ ["{ "] ++ map (\ops -> show ops ++ "; " ) (elems row_ops) ++ [" }"]

instance Show SpanOp where
    show (AttributeChange attr) = show attr
    show (TextSpan width _) = "TextSpan " ++ show width

row_ops_effected_columns :: SpanOpSequence -> [Word]
row_ops_effected_columns spans 
    = map span_ops_effected_columns (elems $ row_ops spans)

all_spans_have_width spans expected
    = all (== expected) ( map span_ops_effected_columns (elems $ row_ops spans) )

span_ops_effected_rows :: SpanOpSequence -> Word
span_ops_effected_rows (SpanOpSequence _ row_ops) 
    = toEnum $ length (filter (not . null) (elems row_ops))
        
type SpanConstructLog = [SpanConstructEvent]
data SpanConstructEvent = SpanSetAttr Attr

is_set_attr :: Attr -> SpanConstructEvent -> Bool
is_set_attr expected_attr (SpanSetAttr in_attr)
    | in_attr == expected_attr = True
is_set_attr _attr _event = False

data DebugWindow = DebugWindow Word Word
    deriving (Show, Eq)

type TestWindow = DebugWindow

instance WinRegion DebugWindow where
    win_width (DebugWindow w _) = w
    win_height (DebugWindow _ h) = h

type ImageConstructLog = [ImageConstructEvent]
data ImageConstructEvent = ImageConstructEvent
    deriving ( Show, Eq )

forward_image_ops = map forward_transform debug_image_ops

forward_transform, reverse_transform :: ImageOp -> (Image -> Image)

forward_transform (ImageOp f _) = f
reverse_transform (ImageOp _ r) = r

data ImageOp = ImageOp ImageEndo ImageEndo
type ImageEndo = Image -> Image

debug_image_ops = 
    [ id_image_op
    -- , render_single_column_char_op
    -- , render_double_column_char_op
    ]

id_image_op :: ImageOp
id_image_op = ImageOp id id

-- render_char_op :: ImageOp
-- render_char_op = ImageOp id id
    
instance Show Picture where
    show (Picture _ image _ ) = "Picture ?? " ++ show image ++ " ??"
