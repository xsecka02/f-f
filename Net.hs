module Net where

-- ---- DATA TYPE DEFINITIONS ---- -- 

data Arc = Arc { source :: Int  
               , target :: Int  
               , capacity :: Int  
               , flow :: Int
               }

data Node = Node { id :: Int
                 , neighbors :: [Arc]
                 }

data Net = Net { src :: Int
               , sink :: Int
               , nodes :: [Node]
               , arcs :: [Arc]
               , maxFlow :: Int
               }

-- --- ---- SHOW FUNCTIONS ---- --- --
    
instance Show Arc where
    show (Arc s t c _ ) = "a " ++ (show s) ++ " " ++ (show t) ++ " " ++ (show c) 

instance Show Net where
    show (Net src snk nodes arcs _ ) = "p max " ++ (show (length nodes)) ++ " " ++ (show (length arcs)) ++ "\n"
                                        ++ "n " ++ (show src) ++ " s\n" 
                                        ++ "n " ++ (show snk) ++ " t\n"
                                        ++ (showL arcs)

showL [] = ""
showL (x:xs) = show x ++ showl xs

showl [] = "" 
showl (x:xs) = "\n" ++ (show x) ++ (showl xs)


printNet :: Net -> IO ()
printNet net = putStrLn (show net)
   
-- ---- ----- FUNCTIONS ----- ---- -- 

