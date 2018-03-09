module Net where

import Prelude hiding (id)
import Data.Map (Map)
import qualified Data.Map as Map

-- ---- DATA TYPE DEFINITIONS ---- -- 

data Arc = Arc { source :: Int  
               , target :: Int  
               , capacity :: Int  
               , flow :: Int
               }

data Net = Net { src :: Int
               , sink :: Int
               , nodes :: Map.Map Int [Arc]
               , arcs :: [Arc]
               , maxFlow :: Int
               }

-- --- ---- INSTANCE FUNCTIONS ---- --- --
    
instance Show Arc where
    show (Arc s t c _ ) = "a " ++ (show s) ++ " " ++ (show t) ++ " " ++ (show c) 

instance Show Net where
    show (Net src snk nodes arcs _ ) = "p max " ++ (show (Map.size nodes)) ++ " " ++ (show (length arcs)) ++ "\n"
                                        ++ "n " ++ (show src) ++ " s\n" 
                                        ++ "n " ++ (show snk) ++ " t\n"
                                        ++ (showL arcs)

-- ---- ----- FUNCTIONS ----- ---- -- 

showL [] = ""
showL (x:xs) = show x ++ showl xs

showl [] = "" 
showl (x:xs) = "\n" ++ (show x) ++ (showl xs)


printNet :: Net -> IO ()
printNet net = putStrLn (show net)
