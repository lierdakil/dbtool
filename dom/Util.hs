module Util where

import Data.Char
import Data.Bits
import Data.Word

import Reflex
import Reflex.Dom
import Reflex.Tags
import qualified Data.Map as M

urlEncode :: String -> String
urlEncode     [] = []
urlEncode (ch:t)
  | (isAscii ch && isAlphaNum ch) || ch `elem` "-_.~" = ch : urlEncode t
  | not (isAscii ch) = foldr escape (urlEncode t) (encodeChar ch)
  | otherwise = escape (fromIntegral (fromEnum ch)) (urlEncode t)
    where
      escape b rs = '%':showH (b `div` 16) (showH (b `mod` 16) rs)

      showH :: Word8 -> String -> String
      showH x xs
        | x <= 9    = to (o_0 + x) : xs
        | otherwise = to (o_A + (x-10)) : xs
        where
          to  = toEnum  .  fromIntegral
          fro = fromIntegral . fromEnum
          o_0 = fro '0'
          o_A = fro 'A'
      encodeChar :: Char -> [Word8]
      encodeChar = map fromIntegral . go . ord
       where
        go oc
         | oc <= 0x7f       = [oc]

         | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                              , 0x80 + oc .&. 0x3f
                              ]

         | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                              , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                              , 0x80 + oc .&. 0x3f
                              ]
         | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                              , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                              , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                              , 0x80 + oc .&. 0x3f
                              ]

graphImg :: MonadWidget t m => String -> m ()
graphImg gvg = do
  let scriptaddr = "http://livid.pp.ru/graphviz.php?input="
      img = scriptaddr ++ urlEncode gvg
  imgDynAttr (constDyn . M.fromList $ [ ("src", img) ]) $ return ()
