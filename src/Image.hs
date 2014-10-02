module Image where

import Utils
import Data.Maybe
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.PE.Structures as PE

newtype RelativeAddress = RelativeAddress Word32 
                          deriving (Show, Eq, Ord)

newtype AbsoluteAddress = AbsoluteAddress Word64
                          deriving (Show, Eq, Ord)

data Section = Section
    { name :: String
    , virtualAddress :: RelativeAddress
    , virtualSize :: Int
    , binaryOffset :: Int
    , binarySize :: Int 
    }
    deriving (Show, Eq)

offsetOf :: RelativeAddress -> RelativeAddress -> Int
offsetOf (RelativeAddress a) (RelativeAddress b) = fromInteger (toInteger a - toInteger b)

class Image i where
    sections :: i -> [Section] 
    entryPoint :: i -> RelativeAddress


offsetInSection addr sec = 
    let ofs = addr `offsetOf` virtualAddress sec
    in if ofs >= 0 && ofs < virtualSize sec
       then Just (ofs + binaryOffset sec)
       else Nothing

rvaToFileOffset :: Image i => i -> RelativeAddress -> Maybe Int
rvaToFileOffset img addr = img 
                         |> sections
                         |> mapMaybe (offsetInSection addr)
                         |> listToMaybe


data PEImage = PEImage { file :: PE.PEFile } deriving (Show)

toSection :: (PE.SectionTable, B.ByteString) -> Section
toSection (st, _) = Section
    { name = PE.sectionHeaderName st
    , virtualAddress = RelativeAddress (PE.virtualAddress st)
    , virtualSize = fromIntegral (PE.virtualSize st)
    , binaryOffset = fromIntegral (PE.pointerToRawData st)
    , binarySize = fromIntegral (PE.sizeOfRawData st)
    }

instance Image PEImage where
    sections img = img
                 |> file
                 |> PE.peHeader
                 |> PE.sectionTables
                 |> map toSection
    entryPoint img = img
                   |> file
                   |> PE.peHeader
                   |> PE.standardFields
                   |> PE.addressOfEntryPoint
                   |> RelativeAddress


