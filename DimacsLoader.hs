module DimacsLoader where
import Net
import Arguments
import System.Exit
import System.Directory

-- ---- DATA TYPE DEFINITIONS ---- -- 

   
-- ---- ----- FUNCTIONS ----- ---- -- 

loadNetwork :: Args -> IO Opts
-- Loads network from given input
loadNetwork (Args m f) = do
    isFile <- doesFileExist f
    if isFile 
        then do
            putStrLn ("reading file " ++ f)
            file <- readFile f
            let fileLines = deleteComments (lines file)
            putStrLn (showL (getArcs fileLines))

        else if (length f) < 1 then putStrLn "Given an invalid input file!"
            else putStrLn "xxx"

    return (Opts m (Net 1 2 [(Node 1 []),(Node 2 [])] [(Arc 1 2 10 10)] 10)) 


deleteComments []     = []
deleteComments (x:xs) = if (head x) == 'c' 
    then deleteComments xs
    else x:(deleteComments xs)

getArcs :: [String] -> [Arc]
getArcs [] = []
getArcs (x:xs) = do
    let record = words x
    if (head x) == "a"  
    then if (length record) == 4 
        then do
            (Arc (record !! 0) (record !! 1) (record !! 2) 0):(getArcs xs)
    else getArcs xs

readNums :: String -> [Int]
readNums = map read . words