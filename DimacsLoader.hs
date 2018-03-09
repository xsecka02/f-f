module DimacsLoader where
import Net
import Arguments
import System.Exit
import System.Directory
import Text.Read
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- ---- DATA TYPE DEFINITIONS ---- -- 

   
-- ---- ----- FUNCTIONS ----- ---- -- 

loadNetwork :: Args -> IO Opts
-- Loads network from given input
loadNetwork (Args m f) = do
    isFile <- doesFileExist f
    if isFile 
        then do
            putStrLn ("Reading file " ++ f)
            file <- readFile f
            let fileLines = deleteComments (lines file)
            let nodeLines = filter (startsWith "n") fileLines
            let sink = getNodeId nodeLines "t"
            let source = getNodeId nodeLines "s"
            let arcs = getArcs (filter (startsWith "a") fileLines)
            if isJust arcs 
                    then putStrLn (showL (fromJust arcs))
                    else putStrLn "Loading arcs was unsuccesfull"
            

        else if (length f) < 1 then putStrLn "Given an invalid input file!"
            else putStrLn "xxx"

    return (Opts m (Net 1 2 (Map.fromList[(1, []),(2, [])]) [(Arc 1 2 10 10)] 10)) 

startsWith :: String -> String -> Bool
startsWith x y = do
    let w = words y
    if (head w) == x 
        then True
        else False

deleteComments []     = []
deleteComments (x:xs) = do
    let line = words x
    if (head line) == "c" 
    then deleteComments xs
    else x:(deleteComments xs)



getNodes :: [Arc] -> Map.Map Int [Arc]
getNodes []     = []
getNodes (x:xs) = do
    let rest = getNodes xs
    []

getNodeId :: [String] -> String -> Maybe Int
getNodeId []       = Nothing
getNodeId (x:xs) v = do
    let w = words x
    if (length w) == 3 && (w !! 0) == "n" && (w !! 2) == v
        then do
            let this = readMaybe (w !! 1)
            if (isJust this) 
                then this
                else getNodeId xs
        else getNodeId xs
                

getArcs :: [String] -> Maybe [Arc]
-- Reads all records in file that should contain arc informations.
-- If there is any invalid arc record, returns nothing.
getArcs []     = Just []
getArcs (x:xs) = do
    let nums = readNums (tail (words x))
    if (length nums) == 3 
        then do 
            let rest = getArcs xs
            if isJust rest 
                then Just ((Arc (nums !! 0) (nums !! 1) (nums !! 2) 0):(fromJust rest))
                else Nothing
        else Nothing
    
    
readNums :: [String] -> [Int]
-- Reads ints from a list of strings, in case the element is not
-- convertable, it is skipped
readNums []     = []
readNums (x:xs) = do
    let num = readMaybe x :: Maybe Int
    if isJust num 
        then ((fromJust num):(readNums xs))
        else readNums xs
    
