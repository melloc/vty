{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Verify.Graphics.Vty.Image
import Graphics.Vty.Debug

import Verify

import Data.Word

is_horiz_text_of_columns :: Image -> Word -> Bool
is_horiz_text_of_columns (HorizText { columns = in_w }) expected_w = in_w == expected_w
is_horiz_text_of_columns (HorizBlank { columns = in_w }) expected_w = in_w == expected_w
is_horiz_text_of_columns _image _expected_w = False

verify_horiz_contat_wo_attr_change_simplifies :: SingleRowSingleAttrImage -> Bool
verify_horiz_contat_wo_attr_change_simplifies (SingleRowSingleAttrImage _attr char_count image) =
    is_horiz_text_of_columns image char_count

verify_horiz_contat_w_attr_change_simplifies :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Bool
verify_horiz_contat_w_attr_change_simplifies (SingleRowSingleAttrImage attr0 char_count0 image0) 
                                             (SingleRowSingleAttrImage attr1 char_count1 image1) 
    | char_count0 == 0 || char_count1 == 0 || attr0 == attr1 = is_horiz_text_of_columns i (char_count0 + char_count1)
    | otherwise = False == is_horiz_text_of_columns i (char_count0 + char_count1)
    where i = image0 <|> image1

main = run_test $ do
    verify "verify_horiz_contat_wo_attr_change_simplifies" verify_horiz_contat_wo_attr_change_simplifies
    verify "verify_horiz_contat_w_attr_change_simplifies" verify_horiz_contat_w_attr_change_simplifies
    return ()
