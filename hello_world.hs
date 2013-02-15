module Main where
import System.Environment

main :: IO ()
main = do
	line <- getLine
	putStrLn (line)
