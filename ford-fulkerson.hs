module Main where

import Net
import Arguments
import System.Environment
import DimacsLoader


main = getArgs >>= parseArgs >>= loadNetwork >>= run

run :: Opts -> IO ()
run (Opts m n) = do
    if (length (nodes n)) < 2 
        then putStrLn "No valid network obtained => END..."
        else if elem PrintNet m then (printNet n) else putStrLn "Valid network obtained, working..." --case m of PrintNet -> (printNet n)
--                       MaxFlowValue -> putStrLn "max flow no."
--                       MaxFlowPath -> putStrLn "path vector"
--                       m -> putStrLn "Invalid options"
--