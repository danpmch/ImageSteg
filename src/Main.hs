module Main where

import System.Environment
import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

data StegArg a = StegArg a a

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
   let filenames = parse args
   files <- sequence . fmap getFiles $ filenames
   let image = files >>= steg >>= encodeDynamicPng
   case image of
        (Right imgData) ->
            LB.writeFile "test.png" imgData
        (Left error) -> putStrLn error

getFiles :: StegArg String -> IO (StegArg B.ByteString)
getFiles = sequence . fmap B.readFile

parse :: [String] -> Either String (StegArg String)
parse (fileToHide : fileToHideIn : _) = Right (StegArg fileToHide fileToHideIn)
parse _ = Left "Usage: ImageSteg  <file to hide>  <file to hide in>"

steg :: StegArg B.ByteString -> Either String Codec.Picture.Types.DynamicImage
steg (StegArg fileToHide fileToHideIn) = decodePng fileToHide

