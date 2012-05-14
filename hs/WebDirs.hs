import System.Directory
import System.Posix.Files
import System.FilePath
import Data.List
import Dirfuncs 
import System.Environment

dodir dirL = do 
	dirs <- getDirs dirL
	files<- getFiles dirL
	fi <- mapM (getFileInfo dirL) files
	mapM_ (putStrLn.show) fi
	mapM_ dodir dirs

main = do 
	args <- getArgs
	dodir (args!!0)
	putStrLn "Done."
