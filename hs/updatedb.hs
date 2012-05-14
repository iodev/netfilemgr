module Main (
main
) where

import System.Environment
import System.Directory
import System.Posix.Files
import System.FilePath
import Data.List

processdir outstream dir = do 
	a <- getDirectoryContents dir
	fs <- mapM (\x -> getFileStatus (dir </> x)) a
	let afs = zip a fs
	    dirnames = map fst (filter (\(x,y) -> isDirectory y) afs)
	    fnames   = map fst (filter (\(x,y) -> (not $ isDirectory y)) afs)
	writeFile outstream joinTab fnames
	map (processdir outstream) dirnames 



main :: IO()
main = do a <- getArgs
	  if (length a < 1) then putStrLn "need location parameter"; return ()
	  let basedir = head a 
	  dlist <- getDirectoryContents basedir
	  
	  mapM_ putStrLn dlist
	  return () 
