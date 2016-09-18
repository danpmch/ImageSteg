module ImageSteg
( Hideable(..)
, hide
, reveal
) where

import Data.Word
import Data.Bits
import Codec.Picture.Types
import qualified Data.List as List
import qualified Data.Vector.Storable as Vec

class Hideable a where
   toWords :: a -> [Word8]
   fromWords :: [Word8] -> Either String ([Word8], a)

hide :: Hideable a => a -> DynamicImage -> Either String DynamicImage

hide message (ImageRGBA8 (Image w' h' v')) =
   Right . ImageRGBA8 $ (Image w' h' newVec)
   where rawMsg = (toWords message)
         (usedHiding, unusedHiding) =
            List.genericSplitAt ((List.length rawMsg) * 4) (Vec.toList v')
         groups = groupify 4 usedHiding
         zipped = zip rawMsg groups
         hidden = map (uncurry hideWord) zipped
         newList = (concat hidden) ++ unusedHiding
         newVec = Vec.fromList newList

hide _ img = Left ("Image to hide in has unsupported file encoding: " ++ (imageToString img))

reveal :: Hideable a => DynamicImage -> Either String ([Word8], a)
reveal (ImageRGBA8 (Image _ _ v)) =
   fromWords words
   where rawMsg = Vec.toList v
         groups = groupify 4 rawMsg
         words = map revealWord groups

hideWord :: Word8 -> [Word8] -> [Word8]
hideWord word rgbs @ [r, g, b, a] =
   map (\ (w, rgb) -> (truncateWord rgb) + w) (zip wordPieces rgbs)
   where wordPieces = wordToList word
hideWord word rgba = error ("hide: Wrong number of values to hide in: " ++ (show . List.length $ rgba))

revealWord :: [Word8] -> Word8
revealWord words @ [w1, w2, w3, w4] =
   let lowBits = map (.&. 3) words
       shifts = (shift `map` lowBits) `zip` [6,4,2,0]
       shiftedBits = map (\ (shiftFunc, shift) -> shiftFunc shift) shifts
       in
       sum shiftedBits
revealWord words = error ("listToWord: unexpected number of words: " ++ (show . List.length $ words))

groupify :: Integral n => n -> [a] -> [[a]]
groupify n xs = case List.genericSplitAt n xs of
                    (xs', []) -> [xs']
                    (xs', rest) -> xs' : (groupify n rest)

wordToList :: Word8 -> [Word8]
wordToList word = do
   bits <- groupify 2 . toBitList $ word
   return (toWord8 bits)

toBitList :: FiniteBits a => a -> [Bool]
toBitList w = let size = finiteBitSize w in
               do
                  index <- [size - 1, (size - 2)..0]
                  let bit = testBit w index
                  return bit

toWord8 :: [Bool] -> Word8
toWord8 [False, False] = 0
toWord8 [False, True] = 1
toWord8 [True, False] = 2
toWord8 [True, True] = 3
toWord8 unknown = error ("toWord8: unexpected input :" ++ (show unknown))

truncateWord :: Word8 -> Word8
truncateWord w = w .&. (complement 3)

imageToString :: DynamicImage -> String
imageToString (ImageY8 _) = "ImageY8"
imageToString (ImageY16 _) = "ImageY16"
imageToString (ImageYF _) = "ImageYF"
imageToString (ImageYA8 _) = "ImageYA8"
imageToString (ImageYA16 _) = "ImageYA16"
imageToString (ImageRGB8 _) = "ImageRGB8"
imageToString (ImageRGB16 _) = "ImageRGB16"
imageToString (ImageRGBF _) = "ImageRGBF"
imageToString (ImageRGBA8 _) = "ImageRGBA8"
imageToString (ImageRGBA16 _) = "ImageRGBA16"
imageToString (ImageCMYK8 _) = "ImageCMYK8"
imageToString (ImageCMYK16 _) = "ImageCMYK16"

