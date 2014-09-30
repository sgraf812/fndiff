module Main where

import Paths_fndiff
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.PE.Parser as PE

builds :: [Int]
builds = [16357, 16826, 17538]

exePath :: Bool -> Int -> IO FilePath
exePath x64 build = getDataFileName ("x86/Wow_" ++ show build ++ ".exe")

x86Exes :: M.Map Int (IO B.ByteString)
x86Exes = 
    let paths = fmap (exePath False) builds
        files = fmap (>>= B.readFile) paths
    in M.fromList (zip builds files)
           
main :: IO ()
main = do
    bits <- fromJust . M.lookup (head builds) $ x86Exes
    
    putStrLn . show . PE.buildFileFromBS $ bits
       
       
