module Dirfuncs (notDot,getDirs,getDirNames
,getLSHFiles
,emptyDir
--,rplaceOut,
,replaceBaseDir
--,getLshOutPairs
,getFiles
,getFileInfo
,FileInfo
  )
where
import System.Directory
import System.Posix.Files
import System.FilePath
import Data.List
import Text.Regex
import Crypto.Hash.SHA1
import Data.ByteString.Lazy as LZ (readFile,fromChunks)
import Data.ByteString.Lazy.Internal
import Data.ByteString.Lazy.UTF8 as LZu
import Data.Hex
import Data.Time
import Data.Time.Clock.POSIX as PClock
import Data.Ord

infoByHash :: [FileInfo] -> [FileInfo]
infoByHash = sortBy (comparing hsh)

infoByPath :: [FileInfo] -> [FileInfo]
infoByPath = sortBy (comparing dirPath)

infoByFileName :: [FileInfo] -> [FileInfo]
infoByFileName = sortBy (comparing fileName)

infoByTime :: [FileInfo] -> [FileInfo]
infoByTime = sortBy (comparing lstWrite)

getHash fname = do
    content <- LZ.readFile fname
    -- to print text:
    return $ hex (fromChunks [hashlazy content])

data FileInfo = FileInfo {
	hsh :: String,
	lstWrite :: UTCTime,
	dirPath :: String,
	fileName :: String
} deriving (Eq,Read,Show)

getFileInfo :: String -> String -> IO FileInfo
getFileInfo strDir strFile = do
	let strPath = strDir </> strFile
	h <- getHash strPath
	w <- getFileTime strPath
	return $ FileInfo { 	hsh = LZu.toString h, 
				lstWrite = w, 
				dirPath = strDir, 
				fileName = strFile }

getFileTime strFPath = do
	fs <- getFileStatus strFPath
	return $ PClock.posixSecondsToUTCTime ((realToFrac (modificationTime fs))::PClock.POSIXTime)

notDot :: [FilePath] -> [FilePath]
notDot = filter (\tst -> ((&&) ((/=) (head tst) '.') ((/=) (last tst) '.')))

getDirs dir = do 
	a <- getDirectoryContents dir
	fs <- mapM (\x -> getFileStatus (dir </> x)) a
	let afs = zip a fs
	    dirnames = map fst (filter (\(x,y) -> isDirectory y) afs)
	return $ notDot (map ((</>) dir) dirnames)

getDirNames dir = do 
	a <- getDirectoryContents dir
	fs <- mapM (\x -> getFileStatus (dir </> x)) a
	let afs = zip a fs
	    dirnames = map fst (filter (\(x,y) -> isDirectory y) afs)
	return dirnames

getLSHFiles dir = do
	a <- getDirectoryContents dir
	fs <- mapM (\x -> getFileStatus (dir </> x)) a
	let afs = zip a fs
	    fnames = map fst (filter (\(x,y) -> ((&&) (not (isDirectory y)) (isSuffixOf "_00.lsh" x))) afs)
	return fnames	

getFiles dir = do
	a <- getDirectoryContents dir
	fs <- mapM (\x -> getFileStatus (dir </> x)) a
	let afs = zip a fs
	    fnames = notDot (map fst (filter (\(x,y) -> (not $ isDirectory y)) afs))
	return fnames

getFileNames = getFiles

emptyDir dir = do
	a <- getDirectoryContents dir
	return $ null (notDot a)

--rplaceOut a =
--	let x = splitPath a
--	in joinPath (((init x)::[FilePath]):[(subRegex (mkRegex "_\\d\\d.lsh") (((last x)!!0)::String) ".out")])

--getLshOutPairs dir = do
--	a <- getLSHFiles dir
--	return $ zip a (map rplaceOut a)
 
replaceBaseDir src oldbase newbase = 
	subRegex (mkRegex oldbase) src newbase

