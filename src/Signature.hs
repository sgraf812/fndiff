{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Signature where

import Image
import Control.FSharp.Syntax.Operators
import Control.Monad
import Data.Serialize
import Data.ByteString as BS
import Data.HList (HList(..), Apply(..), ApplyAB(..), hEnd, hBuild, hMapOut)

type Location = (Disassembly, RelativeAddress)
data Disassembly = forall img . Image img => DA img

class Serialize c => Characteristic c where
    characterize :: Location -> c
    score :: Location -> c -> Float

newtype Hash = MkHash Int deriving (Show)
newtype Counter = MkCounter Int deriving (Show)

instance Serialize Hash where
    get = MkHash `fmap` get
    put (MkHash i) = put i 

instance Serialize Counter where
    get = MkCounter `fmap` get
    put (MkCounter i) = put i 

instance Characteristic Hash where
    characterize _ = MkHash 123
    score _ _ = 0

instance Characteristic Counter where
    characterize _ = MkCounter 5
    score _ _ = 1

loc :: Location
loc = undefined

instance Serialize (HList '[]) where
    get = return HNil
    put HNil = return ()

instance (Serialize e, Serialize (HList l)) => Serialize (HList (e ': l)) where
    get = liftM2 HCons get get
    put (x `HCons` xs) = do 
        _ <- put x
        _ <- put xs
        return ()

class InitLoc a where
    initFromLoc :: Location -> a

instance InitLoc (HList '[]) where
    initFromLoc _ = HNil

instance (Characteristic e, InitLoc (HList l)) => InitLoc (HList (e ': l)) where
    initFromLoc l = characterize l `HCons` initFromLoc l

fingerprint :: HList '[Hash, Counter]
fingerprint = initFromLoc loc

bytes :: BS.ByteString
bytes = fingerprint |> put |> runPut

fingerprint2 :: HList '[Hash, Counter]
fingerprint2 = case runGet get bytes of
                   Right fp -> fp
                   Left e -> error e

data ScoreIfy = HMapScore Location
instance Characteristic c => ApplyAB ScoreIfy c Float where
    applyAB (HMapScore l) = score l

totalScore :: Float
totalScore = hMapOut (HMapScore loc) fingerprint2 |> sum
