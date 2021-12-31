module UniversalTM where

import TM
import Util
import Data.Char

utape :: String
utape = ['0','1','#',',','.']

class UEncode a where
  bitenc :: a -> String -- over utape

bitChar :: Bool -> Char
bitChar True = '1'
bitChar False = '0'

instance UEncode Int where
  bitenc x = if x <= 0 then []
             else
               (bitChar $ x `mod` 2 == 1) : bitenc (x `div` 2)

instance UEncode Char where
  bitenc = bitenc . ord

instance UEncode Direction where
  bitenc GoLeft = "1"
  bitenc GoRight = "0"

instance UEncode Integer where
  bitenc x = bitenc (fromIntegral x :: Int)

instance (UEncode state, UEncode tape) => UEncode (Trans state tape) where
  bitenc (Trans st g d st' g') =
    dot st $
    dot g $
    dot d $
    dot st' $
    dot g' $ ""

{- convenience function to encode a to a string of 1s and 0s,
   and then append another string.  The Char u separates the
   two strings.-}
bitencSep :: UEncode a => Char -> a -> String -> String
bitencSep u x rest = bitenc x ++ (u : rest)

pound :: UEncode a => a -> String -> String
pound = bitencSep '#'
comma :: UEncode a => a -> String -> String
comma = bitencSep ','
dot :: UEncode a => a -> String -> String
dot = bitencSep '.'
blank1 :: UEncode a => a -> String -> String
blank1 = bitencSep ' '

list :: UEncode a => [a] -> String -> String
list xs rest = foldrGlue comma xs ('#' : rest)

encodeh :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape ->
          String {- string to follow this one -} ->
          String -- over Utape
encodeh (TM states inputs tapesyms _ blank leftend trans start final) rest =
  pound leftend $
  pound blank $
  pound start $
  list final $
  list trans rest

encode :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape ->
          String -- over Utape
encode tm = encodeh tm ""

-- turn a TM and an input string into a single input for the universal TM
inputU :: (UEncode input, UEncode state, UEncode tape) =>
          TM input state tape -> [input] -> String -- over Utape
inputU tm xs = encodeh tm (concat(replicate 25 " ") ++ "&`" ++ insertstartandend tm xs) -- added the ` because I didnt want to change my transitions, it gets written over, and the readhead @ is placed in the correct position
                  where insertstartandend (TM states inputs tapesyms _ blank leftend trans start final) xs = extendwords (words (blank1 leftend $ list1 xs $ blank1 blank ""))
                        extendwords [] = []
                        extendwords (a : as) | length a < 10 = a ++ concat(replicate (10 - length a) "0") ++ "," ++ extendwords as
                                             | otherwise = a ++ "," ++ extendwords as

-- >>> list1 "aaaa" ""
-- "1000011 1000011 1000011 1000011 "

list1:: UEncode a => [a] -> String -> String
list1 xs rest = foldrGlue blank1 xs rest
-- >>> list1 "aaaa" ""
-- "1000011,1000011,1000011,1000011,"
