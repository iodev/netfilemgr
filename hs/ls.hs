module Main (
main
) where

import System.Environment
import System.Directory

main :: IO()
main = do a <- getArgs
	  dlist <- getDirectoryContents $ (if (length a > 0) then head a else ".")
	  mapM_ putStrLn dlist
	  return () 
