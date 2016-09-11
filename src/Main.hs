module Main where

import System.Environment
import Codec.Picture.Png

main :: IO ()
main = do
   args <- getArgs
   let files = parse args
       result = fmap (uncurry steg) files
   case result of
        (Right result) -> putStrLn result
        (Left error) -> putStrLn error

parse :: [String] -> Either String (String, String)
parse (fileToHide : fileToHideIn : _) = Right (fileToHide, fileToHideIn)
parse _ = Left "Usage: ImageSteg  <file to hide>  <file to hide in>"

steg :: String -> String -> String
steg _ _ = "TODO"

