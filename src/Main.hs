module Main where

import Paths_fndiff
import Image
import Signature
import Disasm
import Control.FSharp.Syntax.Operators
import Data.Maybe (fromJust)
import Numeric (showHex)
import Hdis86.Pure 
import Hdis86.Types
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.PE.Parser as PE
import qualified Data.PE.Structures as PE

builds :: [Int]
builds = [16357, 16826, 17538]

exePath :: Bool -> Int -> IO FilePath
exePath x64 build = getDataFileName ("x86/Wow_" ++ show build ++ ".exe")

x86Exes :: M.Map Int (IO B.ByteString)
x86Exes = 
    let paths = fmap (exePath False) builds
        files = fmap (>>= B.readFile) paths
    in M.fromList (zip builds files)

hex :: B.ByteString -> String
hex = foldl' (flip showHex) "" . B.unpack

config :: Config
config = Config Intel Mode32 SyntaxIntel 0x400000

main :: IO ()
main = do
    bits <- exePath False (head builds) >>= B.readFile
    let image = PEImage (PE.buildFileFromBS bits)
    let pep = image |> pe |> PE.sectionTables
    print pep
    mapM_ print (sections image)
    print (entryPoint image)
    let ofs = image |> entryPoint |> rvaToBinaryOffset image |> fromJust |> fromIntegral
    print ofs
    let entry = B.drop ofs bits
    print . hex . B.take 20 $ entry
    mapM_ (print . mdInst) . disassembleMetadata config . B.toStrict $ entry

       
