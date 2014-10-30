module Disasm where

import Image
import Control.FSharp.Syntax.Operators
import Data.Bits
import Data.Word
import Data.Vector.Unboxed.Mutable
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

data BasicBlock = BasicBlock 
    { start :: RelativeAddress
    , size :: Int
    , calls :: [RelativeAddress]
    , branches :: [RelativeAddress] -- TODO: Specialize on 0,1,2,n branches
    , isFunction :: Bool
    } deriving (Eq, Show)

end :: BasicBlock -> RelativeAddress
end bb = bb |> start |> advance (size bb)

type BlockMap = HM.HashMap RelativeAddress BasicBlock

data Disassembly = DA BlockMap

disassemble :: Image -> passes -> Disassembly
disassemble img _ = recurse img [(entryPoint img, True)] |> DA

type DisassemblyRoot = (RelativeAddress, Bool) -- address and flag indicating if it is the start of a function

recurse :: Image -> [DisassemblyRoot] -> BlockMap
recurse _ [] = HM.empty
recurse img ((r, isFn):roots) =
    let (sec, ofs) = containingSection img r |> fromJust -- TODO: handle Nothing, e.g. an AV
        blockBytes = sec |> bytes |> BS.drop (fromIntegral ofs) -- disassemble this
        block = analyzeBlock img (r, isFn) blockBytes
        cs = block |> calls |> fmap (\c -> (c, True))
        bs = block |> branches |> fmap (\b -> (b, False))
    in recurse img (roots ++ bs ++ cs) |> HM.insert r block
        

analyzeBlock :: Image -> DisassemblyRoot -> BS.ByteString -> BasicBlock
analyzeBlock img (r, isFn) bytes = 
    let sdf = undefined
    in BasicBlock
    { start = r
    , isFunction = isFn
    }

--exploreLinearly :: (Image img) => img ->
