{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Verify.Graphics.Vty.Image ( module Verify.Graphics.Vty.Image
                                 , module Graphics.Vty.Image
                                 )
    where

import Verify.Graphics.Vty.Attributes
import Graphics.Vty.Image
import Graphics.Vty.Image.Internal

import Verify

data UnitImage = UnitImage Char Image

instance Arbitrary UnitImage where
    arbitrary = do
        SingleColumnChar c <- arbitrary
        a <- arbitrary
        return $ UnitImage c (char a c)

instance Show UnitImage where
    show (UnitImage c _) = "UnitImage " ++ show c

data DefaultImage = DefaultImage Image

instance Show DefaultImage where
    show (DefaultImage i) 
        = "DefaultImage (" ++ show i ++ ") " ++ show (image_width i, image_height i)

instance Arbitrary DefaultImage where
    arbitrary = do
        i <- return $ char def_attr 'X'
        return $ DefaultImage i

data SingleRowSingleAttrImage 
    = SingleRowSingleAttrImage 
      { expected_attr :: Attr
      , expected_columns :: Int
      , row_image :: Image
      }

instance Show SingleRowSingleAttrImage where
    show (SingleRowSingleAttrImage attr columns image) 
        = "SingleRowSingleAttrImage (" ++ show attr ++ ") " ++ show columns ++ " ( " ++ show image ++ " )"

newtype WidthResize = WidthResize (Image -> (Image, Int))

instance Arbitrary WidthResize where
    arbitrary = do
        WidthResize f <- arbitrary
        w <- choose (1,64)
        oneof $ map (return . WidthResize)
            [ \i -> (i, image_width i)
            , \i -> (resize_width w $ fst $ f i, w)
            , \i -> let i' = fst $ f i in (crop_left w i', min (image_width i') w)
            , \i -> let i' = fst $ f i in (crop_right w i', min (image_width i') w)
            ]

newtype HeightResize = HeightResize (Image -> (Image, Int))

instance Arbitrary HeightResize where
    arbitrary = do
        HeightResize f <- arbitrary
        h <- choose (1,64)
        oneof $ map (return . HeightResize)
            [ \i -> (i, image_height i)
            , \i -> (resize_height h $ fst $ f i, h)
            , \i -> let i' = fst $ f i in (crop_top h i', min (image_height i') h)
            , \i -> let i' = fst $ f i in (crop_bottom h i', min (image_height i') h)
            ]

newtype ImageResize = ImageResize (Image -> (Image, (Int, Int)))

instance Arbitrary ImageResize where
    arbitrary = oneof
        [ return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , do
            ImageResize f <- arbitrary
            WidthResize g <- arbitrary
            return $! ImageResize $! \i -> 
                let (i_0, (_, out_height)) = f i
                    g_i = g i_0
                in (fst g_i, (snd g_i, out_height))
        , do
            ImageResize f <- arbitrary
            HeightResize g <- arbitrary
            return $! ImageResize $! \i -> 
                let (i_0, (out_width, _)) = f i
                    g_i = g i_0
                in (fst g_i, (out_width, snd g_i))
        ]


instance Arbitrary SingleRowSingleAttrImage where
    arbitrary = do
        -- The text must contain at least one character. Otherwise the image simplifies to the
        -- IdImage which has a height of 0. If this is to represent a single row then the height
        -- must be 1
        single_column_row_text <- Verify.resize 16 (listOf1 arbitrary)
        a <- arbitrary
        let out_image = horiz_cat $ [char a c | SingleColumnChar c <- single_column_row_text]
            out_width = length single_column_row_text
        return $ SingleRowSingleAttrImage a out_width out_image

data SingleRowTwoAttrImage 
    = SingleRowTwoAttrImage 
    { part_0 :: SingleRowSingleAttrImage
    , part_1 :: SingleRowSingleAttrImage
    , join_image :: Image
    } deriving Show

instance Arbitrary SingleRowTwoAttrImage where
    arbitrary = do
        p0 <- arbitrary
        p1 <- arbitrary
        return $ SingleRowTwoAttrImage p0 p1 (row_image p0 <|> row_image p1)

data SingleAttrSingleSpanStack = SingleAttrSingleSpanStack 
    { stack_image :: Image 
    , stack_source_images :: [SingleRowSingleAttrImage]
    , stack_width :: Int
    , stack_height :: Int
    }
    deriving Show

instance Arbitrary SingleAttrSingleSpanStack where
    arbitrary = do
        image_list <- Verify.resize 16 (listOf1 arbitrary)
        return $ mk_single_attr_single_span_stack image_list
    shrink s = do
        image_list <- shrink $ stack_source_images s
        if null image_list
            then []
            else return $ mk_single_attr_single_span_stack image_list

mk_single_attr_single_span_stack image_list =
    let image = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- image_list ]
    in SingleAttrSingleSpanStack image image_list (maximum $ map expected_columns image_list)
                                                  (toEnum $ length image_list)

instance Arbitrary Image  where
    arbitrary = oneof
        [ return EmptyImage
        , do
            SingleAttrSingleSpanStack {stack_image} <- Verify.resize 8 arbitrary
            ImageResize f <- Verify.resize 2 arbitrary
            return $! fst $! f stack_image
        , do
            SingleAttrSingleSpanStack {stack_image} <- Verify.resize 8 arbitrary
            ImageResize f <- Verify.resize 2 arbitrary
            return $! fst $! f stack_image
        , do
            i_0 <- arbitrary
            i_1 <- arbitrary
            let i = i_0 <|> i_1
            ImageResize f <- Verify.resize 2 arbitrary
            return $! fst $! f i
        , do
            i_0 <- arbitrary
            i_1 <- arbitrary
            let i = i_0 <-> i_1
            ImageResize f <- Verify.resize 2 arbitrary
            return $! fst $! f i
        ]
    {-
    shrink i@(HorizJoin {part_left, part_right}) = do
        let !i_alt = background_fill (image_width i) (image_height i)
        !part_left' <- shrink part_left
        !part_right' <- shrink part_right
        [i_alt, part_left' <|> part_right']
    shrink i@(VertJoin {part_top, part_bottom}) = do
        let !i_alt = background_fill (image_width i) (image_height i)
        !part_top' <- shrink part_top
        !part_bottom' <- shrink part_bottom
        [i_alt, part_top' <-> part_bottom']
    shrink i@(CropRight {cropped_image, output_width}) = do
        let !i_alt = background_fill (image_width i) (image_height i)
        [i_alt, cropped_image]
    shrink i@(CropLeft {cropped_image, left_skip, output_width}) = do
        let !i_alt = background_fill (image_width i) (image_height i)
        [i_alt, cropped_image]
    shrink i@(CropBottom {cropped_image, output_height}) = do
        let !i_alt = background_fill (image_width i) (image_height i)
        [i_alt, cropped_image]
    shrink i@(CropTop {cropped_image, top_skip, output_height}) = do
        let !i_alt = background_fill (image_width i) (image_height i)
        [i_alt, cropped_image]
    shrink i = [empty_image, background_fill (image_width i) (image_height i)]
    -}

data CropOperation
    = CropFromLeft
    | CropFromRight
    | CropFromTop
    | CropFromBottom
    deriving (Eq, Show)

instance Arbitrary CropOperation where
    arbitrary = oneof $ map return [CropFromLeft, CropFromRight, CropFromTop, CropFromBottom]

data Translation = Translation Image (Int, Int) Image
    deriving (Eq, Show)

instance Arbitrary Translation where
    arbitrary = do
        i <- arbitrary
        x <- arbitrary `suchThat` (> 0)
        y <- arbitrary `suchThat` (> 0)
        let i' = translate x y i
        return $ Translation i (x,y) i'

