module Main where

import ImageSteg

import System.Environment
import System.FilePath.Posix
import Codec.Picture.Png
import Codec.Picture.Types
import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import Debug.Trace

data StegArg a = StegArg a a

instance Functor StegArg where
   fmap f (StegArg a1 a2) = StegArg (f a1) (f a2)

instance Foldable StegArg where
   foldr f acc (StegArg a1 a2) = a1 `f` (a2 `f` acc)
   foldMap f (StegArg a1 a2) = mempty `mappend` (f a1) `mappend` (f a2)

instance Traversable StegArg where
   traverse f = sequenceA . fmap f
   sequenceA (StegArg fa1 fa2) = (pure StegArg) <*> fa1 <*> fa2

data Action = Hide (StegArg File)
            | UnHide File

data File = File { filename :: String
                 , content :: LB.ByteString
                 }

instance Hideable File where
   toWords file = ( nameLength ++ name ) ++ ( fileLength ++ fileContent )
      where rawName = filename file
            rawNameLength = length rawName
            nameLength = intToWord8 rawNameLength
            name = B.unpack . C.pack . take rawNameLength $ rawName
            fileContent = LB.unpack . content $ file
            fileLength = intToWord8 . length $ fileContent

   fromWords [] = Left "Not enough data to reconstruct file"
   fromWords _ | trace "fromWords" False = undefined
   fromWords words = do
      (words', nameLength) <- trace "Getting nameLength" (let result @ (w, i) = extractInt words in
                                  {-if List.length w < i
                                     then fail
                                     else-} Right result)
      (name, words'') <- trace "Getting name" (let (n, w) = splitAt nameLength words' in
                             {-if w == []
                                then fail
                                else -}let name = C.unpack . B.pack $ n in
                                         Right (name, w))
      (words''', fileLength) <- trace "Getting fileLength" (let result @ (words, i) = extractInt words'' in
                                    {-if List.length words < i
                                       then fail
                                       else -}Right result)
      let (c, remainder) = trace "Getting content" (splitAt fileLength words''')
          content = LB.pack c
      return (remainder, File name content)
      where fail = Left "Not enough data to reconstruct file"

getFile :: String -> IO File
getFile filename = do
   content <- LB.readFile filename
   return (File filename content)

fileToPng :: File -> Either String DynamicImage
fileToPng = decodePng . B.concat . LB.toChunks . content

main :: IO ()
main = do
   args <- getArgs
   files <- mapM getFile args
   let action = parse files
   case action of
        (Right (Hide filenames)) -> doHide filenames
        (Right (UnHide filename)) -> doUnHide filename
        (Left error) -> putStrLn error

doHide :: StegArg File -> IO ()
doHide _ | trace "doHide" False = undefined
doHide files = do
   let image = steg files
       encodedImage = image >>= encodeDynamicPng
   case encodedImage of
        (Right imgData) ->
            LB.writeFile "hidden.png" imgData
        (Left error) -> putStrLn error

doUnHide :: File -> IO ()
doUnHide _ | trace "doUnHide" False = undefined
doUnHide file = do
   let unhiddenFile = unsteg file
   case unhiddenFile of
        (Right (File name content)) ->
           LB.writeFile (takeFileName name) content
        (Left error) -> putStrLn error

parse :: [File] -> Either String Action
parse [] = Left "Usage: ImageSteg  <file to hide>  <file to hide in>"
parse [file] = Right . UnHide $ file
parse (fileToHide : fileToHideIn : _) = Right . Hide $ (StegArg fileToHide fileToHideIn)

steg :: StegArg File -> Either String DynamicImage
steg (StegArg fileToHide fileToHideIn) = do
   hidingPlace <- fileToPng fileToHideIn
   hide fileToHide hidingPlace


unsteg :: File -> Either String File
unsteg _ | trace "unsteg" False = undefined
unsteg file = do
   img <- fileToPng file
   (_, result) <- reveal img
   return result

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
       totalWords = length words
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
       (intWords, rest) = splitAt totalWords words
       in
       ( rest, (wordsToInt intWords) )

