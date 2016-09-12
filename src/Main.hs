module Main where

import System.Environment
import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.List as List
import Data.Word
import Data.Bits
import qualified Data.Vector.Storable as Vec
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

data StegArg a = StegArg a a

data Action = Hide (StegArg String)
            | UnHide String

instance Functor StegArg where
   fmap f (StegArg a1 a2) = StegArg (f a1) (f a2)

instance Foldable StegArg where
   foldr f acc (StegArg a1 a2) = a1 `f` (a2 `f` acc)
   foldMap f (StegArg a1 a2) = mempty `mappend` (f a1) `mappend` (f a2)

instance Traversable StegArg where
   traverse f = sequenceA . fmap f
   sequenceA (StegArg fa1 fa2) = (pure StegArg) <*> fa1 <*> fa2

main :: IO ()
main = do
   args <- getArgs
   let action = parse args
   case action of
        (Right (Hide filenames)) -> doHide filenames
        (Right (UnHide filename)) -> doUnHide filename
        (Left error) -> putStrLn error

doHide :: StegArg String -> IO ()
doHide filenames = do
   files <- getFiles filenames
   let image = steg files
       encodedImage = image >>= encodeDynamicPng
   case image of
        (Right img) -> putStrLn . imageToString $ img
        _ -> return ()
   case encodedImage of
        (Right imgData) ->
            LB.writeFile "test.png" imgData
        (Left error) -> putStrLn error

doUnHide :: String -> IO ()
doUnHide filename = do
   file <- B.readFile filename
   let image = unsteg file
       encodedImage = image >>= encodeDynamicPng
   case encodedImage of
        (Right imgData) ->
           LB.writeFile "untest.png" imgData
        (Left error) -> putStrLn error

getFiles :: StegArg String -> IO (StegArg B.ByteString)
getFiles = sequence . fmap B.readFile

parse :: [String] -> Either String Action
parse (fileToHide : fileToHideIn : _) = Right . Hide $ (StegArg fileToHide fileToHideIn)
parse [fileToUnhide] = Right . UnHide $ fileToUnhide
parse _ = Left "Usage: ImageSteg  <file to hide>  <file to hide in>"

steg :: StegArg B.ByteString -> Either String DynamicImage
steg (StegArg fileToHide fileToHideIn) = do
   hide <- decodePng fileToHide
   hidingPlace <- decodePng fileToHideIn
   return (store hide hidingPlace)
   where store :: DynamicImage -> DynamicImage -> DynamicImage
         store (ImageRGBA8 (Image w h v)) (ImageRGBA8 (Image w' h' v')) =
            let (usedHiding, unusedHiding) = List.genericSplitAt ((Vec.length v) * 4) (Vec.toList v')
                groups = groupify 4 usedHiding
                zipped = zip (Vec.toList v) groups
                hidden = map (\ (value, storage) ->
                   let vals = wordToList value in
                       hide vals storage
                   ) zipped
                newList = (concat hidden) ++ unusedHiding
                newVec = Vec.fromList newList
                in
                ImageRGBA8 (Image w' h' newVec)

unsteg :: B.ByteString -> Either String DynamicImage
unsteg filename = do
   file <- decodePng filename
   return (unhide file)
   where unhide :: DynamicImage -> DynamicImage
         unhide (ImageRGBA8 (Image w h v)) =
            let groups = groupify 4 (Vec.toList v)
                words = map unhideWord groups
                newVec = Vec.fromList words
                in
                ImageRGBA8 (Image w h newVec)

wordToList :: Word8 -> [Word8]
wordToList word = do
   bits <- groupify 2 . toBitList $ word
   return (toWord8 bits)

toWord8 :: [Bool] -> Word8
toWord8 [False, False] = 0
toWord8 [False, True] = 1
toWord8 [True, False] = 2
toWord8 [True, True] = 3
toWord8 unknown = error ("toWord8: unexpected input :" ++ (show unknown))

hide :: [Word8] -> [Word8] -> [Word8]
hide ws @ [w1, w2, w3, w4] rgbs @ [r, g, b, a] =
   map (\ (w, rgb) -> (truncateWord rgb) + w) (zip ws rgbs)
hide ws [r, g, b, a] = error ("hide: Wrong number of values to hide: " ++ (show . List.length $ ws))
hide [w1, w2, w3, w4] rgba = error ("hide: Wrong number of values to hide in: " ++ (show . List.length $ rgba))

unhideWord :: [Word8] -> Word8
unhideWord words @ [w1, w2, w3, w4] =
   let lowBits = map (.&. 3) words
       shifts = (shift `map` lowBits) `zip` [6,4,2,0]
       shiftedBits = map (\ (shiftFunc, shift) -> shiftFunc shift) shifts
       in
       sum shiftedBits
unhideWord words = error ("listToWord: unexpected number of words: " ++ (show . List.length $ words))


truncateWord :: Word8 -> Word8
truncateWord w = w .&. (complement 3)

groupify :: Integral n => n -> [a] -> [[a]]
groupify n xs = case List.genericSplitAt n xs of
                    (xs', []) -> [xs']
                    (xs', rest) -> xs' : (groupify n rest)

toBitList :: FiniteBits a => a -> [Bool]
toBitList w = let size = finiteBitSize w in
               do
                  index <- [size - 1, (size - 2)..0]
                  let bit = testBit w index
                  return bit

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

