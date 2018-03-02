module Arguments where

import System.Environment
import System.Directory
import System.Exit
import Net

-- ---- DATA TYPE DEFINITIONS ---- -- 

data Mode = PrintNet | MaxFlowValue | MaxFlowPath deriving (Eq)

data Args = Args { options :: [Mode]
                 , filename :: String
            }

data Opts = Opts { runModes :: [Mode]
                 , net :: Net
            }

-- ---- ----- FUNCTIONS ----- ---- --

parseArgs :: [String] -> IO Args
-- Parses mode-options and filename of input

parseArgs [] = help >> return (Args [] "")
parseArgs x  = (parseA x [] "")

parseA :: [String] -> [Mode] -> String -> IO Args
parseA [] o f = return (Args o f)
parseA (x:xs) o f = do 
    isFile <- doesFileExist x 
    if isFile 
        then (parseA xs o x)
        else (parseA xs (addOpt x o) f)

    
    
-- Returns selected run-mode
addOpt o opts = do
    case o of     
        "-i" -> PrintNet:opts
                        
        "-v" -> MaxFlowPath:opts
                
        "-f" -> MaxFlowValue:opts
            
        o -> opts

invalidArgument = putStrLn "Given option or filename is invalid!" >> help
help = putStrLn "Printing help"