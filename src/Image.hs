module Image where

import Data.Maybe
import Data.Word
import Numeric (showHex)
import Data.Hashable (Hashable(..))
import Control.FSharp.Syntax.Operators
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.PE.Structures as PE

newtype RelativeAddress = RelativeAddress Word32 
                          deriving (Eq, Ord)

newtype AbsoluteAddress = AbsoluteAddress Word64
                          deriving (Eq, Ord)

instance Show RelativeAddress where
    show (RelativeAddress addr) = "+0x" ++ showHex addr ""

instance Show AbsoluteAddress where
    show (AbsoluteAddress addr) = "@0x" ++ showHex addr ""

instance Hashable RelativeAddress where
    hashWithSalt s (RelativeAddress addr) = hashWithSalt s addr

instance Hashable AbsoluteAddress where
    hashWithSalt s (AbsoluteAddress addr) = hashWithSalt s addr

data Section = Section
    { name :: String
    , virtualAddress :: RelativeAddress
    , virtualSize :: Int
    , binaryOffset :: Int
    , binarySize :: Int 
    , bytes :: BS.ByteString
    } deriving (Show, Eq)

offsetFrom :: RelativeAddress -> RelativeAddress -> Int
offsetFrom (RelativeAddress a) (RelativeAddress b) = fromIntegral a - fromIntegral b

advance :: Int -> RelativeAddress -> RelativeAddress
advance ofs (RelativeAddress a) = RelativeAddress (a + fromIntegral ofs)

baseAt :: AbsoluteAddress -> RelativeAddress -> AbsoluteAddress
baseAt (AbsoluteAddress addr) (RelativeAddress ofs) = AbsoluteAddress (addr + fromIntegral ofs)

data Image = Image 
    { sections :: [Section]
    , entryPoint :: RelativeAddress
    , baseAddress :: AbsoluteAddress
    , is64Bit :: Bool -- TODO: Maybe an enum
    } deriving Show

offsetInSection :: RelativeAddress -> Section -> Maybe (Section, Int)
offsetInSection addr sec = 
    let ofs = addr `offsetFrom` virtualAddress sec
    in if ofs >= 0 && ofs < virtualSize sec
       then Just (sec, ofs)
       else Nothing

containingSection :: Image -> RelativeAddress -> Maybe (Section, Int)
containingSection img addr = 
    img |> sections
        |> mapMaybe (offsetInSection addr) 
        |> listToMaybe -- safeHead

-- finds the section containing rva addr and returns the offset of addr within it.
-- If no section is found, return Nothing.
rvaToBinaryOffset :: Image -> RelativeAddress -> Maybe Int
rvaToBinaryOffset img addr = do
    (sec, ofs) <- containingSection img addr :: Maybe (Section, Int)
    let binOfs = binaryOffset sec + ofs
    return binOfs

-- finds the sectino containing the entry point of the image (which may fail, hence Maybe)
-- and returns the offset of addr within the section, regardless of addr pointing into the
-- text section or not.
rvaToTextOffset :: Image -> RelativeAddress -> Maybe Int
rvaToTextOffset img addr = do
    let ep = entryPoint img
    (text, _) <- containingSection img ep
    let textOfs = addr `offsetFrom` virtualAddress text
    return textOfs

toSection :: (PE.SectionTable, BL.ByteString) -> Section
toSection (st, bs) = Section
    { name = PE.sectionHeaderName st
    , virtualAddress = RelativeAddress (PE.virtualAddress st)
    , virtualSize = fromIntegral (PE.virtualSize st)
    , binaryOffset = fromIntegral (PE.pointerToRawData st)
    , binarySize = fromIntegral (PE.sizeOfRawData st)
    , bytes = BL.toStrict bs
    }

peImage :: PE.PEFile -> Image
peImage file = 
    let pe = file |> PE.peHeader 
    in Image
    { sections = pe |> PE.sectionTables |> map toSection
    , entryPoint = pe |> PE.standardFields |> PE.addressOfEntryPoint |> RelativeAddress
    , baseAddress = pe |> PE.standardFields |> PE.baseOfCode |> fromIntegral |> AbsoluteAddress
    , is64Bit = case pe |> PE.coffHeader |> PE.targetMachine of
                    PE.AMD64 -> True
                    PE.I386 -> False
                    _ -> error "Only AMD64 and I386 are supported."
    }
        

