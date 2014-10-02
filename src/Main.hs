module Main where

import Paths_fndiff
import Utils
import Image
import Data.Maybe
import Numeric (showHex)
import Data.List (foldl')
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

hex = foldl' (flip showHex) "" . B.unpack
           
main :: IO ()
main = do
    bits <- exePath False (head builds) >>= B.readFile
    let image = PEImage (PE.buildFileFromBS bits)
    print (sections image)
    let ofs = image |> entryPoint |> rvaToFileOffset image |> fromJust |> fromIntegral
    print ofs
    print . hex . B.take 20 . B.drop ofs $ bits
       
       
