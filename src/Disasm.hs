module Disasm where

import Image
import Control.FSharp.Syntax.Operators
import Data.Bits
import Data.Word
import Data.Vector.Unboxed.Mutable
import Data.Maybe (fromJust, listToMaybe)
import Hdis86.Pure (disassembleMetadata) 
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Hdis86.Types as H

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

analyze :: Image -> passes -> Disassembly
analyze img _ = recurse img [(entryPoint img, True)] |> DA

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
    let cfg = if is64Bit img then H.intel64 else H.intel32
        absoluteRoot = baseAt (baseAddress img) r 
        instructions = bytes |> disassembleMetadata (startAt absoluteRoot H.Intel)
                             |> takeWhileIncl (not . isEndOfBlock . H.mdInst)
        updateBlock blk meta = 
            let inst = H.mdInst meta
                len = H.mdLength meta
            in blk 
               { size = size blk + len }
    in foldr BasicBlock
    { start = r
    , isFunction = isFn
    }

startAt :: AbsoluteAddress -> H.CPUMode -> H.Config
startAt (AbsoluteAddress addr) mode = H.Config H.Intel mode H.SyntaxNone addr

data BranchLocation = Immediate AbsoluteAddress
                    | IndirectMem AbsoluteAddress
                    | IndirectReg
                    | SwitchMem AbsoluteAddress
                    | SwitchReg AbsoluteAddress
                    deriving Show

data BranchType = Call { callee :: BranchLocation }
                | Jump { taken :: BranchLocation }
                | Return
                | Conditional { taken :: BranchLocation, notTaken :: AbsoluteAddress }
                deriving Show

data InstData = InstData
              { branch :: Maybe BranchType
              , instSize :: Int
              } deriving Show

extractInstData :: H.Metadata -> InstData
extractInstData meta = InstData
    { branch = mapBranchType meta
    , instSize = meta |> H.mdLength |> fromIntegral
    }

returnInst :: [H.Opcode]
returnInst = [ H.Iret
             , H.Iretf
             , H.Iiretw
             , H.Iiretd
             , H.Iiretd
             ]

conditionalInst :: [H.Opcode]
conditionalInst = [ H.Ija
                  , H.Ijae
                  , H.Ijb
                  , H.Ijbe
                  , H.Ijcxz
                  , H.Ijecxz
                  , H.Ijrcxz
                  , H.Ijg
                  , H.Ijge
                  , H.Ijl
                  , H.Ijle
                  , H.Ijo
                  , H.Ijp
                  , H.Ijs
                  , H.Ijno
                  , H.Ijnp
                  , H.Ijns
                  , H.Ijnz
                  , H.Ijz
                  ]

mapBranchType :: H.Metadata -> Maybe BranchType
mapBranchType meta =
    let inst = H.mdInst meta
        cur = H.mdOffset meta
        len = H.mdLength meta
        after = AbsoluteAddress (cur + fromIntegral len)
        bl = inst |> H.inOperands |> listToMaybe |> (>>= determineBranchLocation after)
    in case H.inOpcode inst of
        H.Ijmp -> Just (Jump bl)
        H.Icall -> Just (Call bl)
        x | x `elem` returnInst -> Just Return
          | x `elem` conditionalInst -> Just (Conditional bl after)
        _ -> Nothing

determineBranchLocation :: AbsoluteAddress -> H.Operand -> Maybe BranchLocation
determineBranchLocation (AbsoluteAddress after) op = undefined
determineBranchLocation _ _ = Nothing


takeWhileIncl :: (a -> Bool) -> [a] -> [a]
takeWhileIncl p (x:xs) = x : if p x then takeWhileIncl p xs else []
takeWhileIncl _ [] = []

isEndOfBlock :: H.Instruction -> Bool
isEndOfBlock instr = H.inOpcode instr `elem` [H.Ijmp, H.Iret]

--exploreLinearly :: Image ->
