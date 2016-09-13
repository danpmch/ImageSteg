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
            LB.writeFile "hidden.png" imgData
        (Left error) -> putStrLn error

doUnHide :: String -> IO ()
doUnHide filename = do
   file <- B.readFile filename
   let image = unsteg file
       encodedImage = image >>= encodeDynamicPng
   case encodedImage of
        (Right imgData) ->
           LB.writeFile "unhidden.png" imgData
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
   store hide hidingPlace
   where store :: DynamicImage -> DynamicImage -> Either String DynamicImage
         store (ImageRGBA8 (Image w h v)) (ImageRGBA8 (Image w' h' v')) =
            let listV = Vec.toList v
                encodedWidth = intToWord8 w
                encodedHeight = intToWord8 h
                augmentedV = encodedWidth ++ encodedHeight ++ listV
                (usedHiding, unusedHiding) = List.genericSplitAt ((List.length augmentedV) * 4) (Vec.toList v')
                groups = groupify 4 usedHiding
                zipped = zip augmentedV groups
                hidden = map (\ (value, storage) ->
                   let vals = wordToList value in
                       hide vals storage
                   ) zipped
                newList = (concat hidden) ++ unusedHiding
                newVec = Vec.fromList newList
                in
                Right . ImageRGBA8 $ (Image w' h' newVec)
         store img (ImageRGBA8 _) = Left ("Image to hide has unsupported file encoding: " ++ (imageToString img))
         store (ImageRGBA8 _) img = Left ("Image to hide in has unsupported file encoding: " ++ (imageToString img))

unsteg :: B.ByteString -> Either String DynamicImage
unsteg filename = do
   file <- decodePng filename
   return (unhide file)
   where unhide :: DynamicImage -> DynamicImage
         unhide (ImageRGBA8 (Image _ _ v)) =
            let listV = Vec.toList v
                groups = groupify 4 listV
                words = map unhideWord groups
                (words', width) = extractInt words
                (words'', height) = extractInt words'
                newVec = Vec.fromList words''
                in
                ImageRGBA8 (Image width height newVec)

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

intToWord8 :: Int -> [Word8]
intToWord8 i =
   let totalBits = finiteBitSize i
       word8bits = finiteBitSize (0 :: Word8)
       shifts = [(totalBits - (1 * word8bits)),(totalBits - (2 * word8bits))..0]
       in
       map (\ shiftVal -> fromIntegral (shift i (-shiftVal)) ) shifts

wordsToInt :: [Word8] -> Int
wordsToInt words =
   let word8bits = finiteBitSize (0 :: Word8)
       totalWords = List.length words
       shifts = [(totalWords - 1) * word8bits, ((totalWords - 2) * word8bits)..0]
       wordShifts = (zip words shifts)
       intWords = map (\ (word, shiftVal) -> shift (fromIntegral word) shiftVal) wordShifts
       in
       sum intWords

extractInt :: [Word8] -> ([Word8], Int)
extractInt words =
   let totalBits = finiteBitSize (0 :: Int)
       word8bits = finiteBitSize (0 :: Word8)
       totalWords = totalBits `quot` word8bits
       (intWords, rest) = List.genericSplitAt totalWords words
       in
       ( rest, (wordsToInt intWords) )


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

