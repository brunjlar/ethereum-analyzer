{-# LANGUAGE OverloadedStrings, FlexibleContexts,
  FlexibleInstances, GADTs, Rank2Types, DeriveGeneric #-}

module Blockchain.Analyze.IR
  ( HplBody
  , HplOp
  , WordLabelMapM
  , evmOps2HplBody
  ) where

import Blockchain.ExtWord as BE
import Blockchain.VM.Opcodes as BVO
import Compiler.Hoopl as CH
import Control.Monad as CM
import Data.Bimap as DB

data HplOp e x where
        CoOp :: Label -> HplOp C O
        OoOp :: (Word256, Operation) -> HplOp O O
        OcOp :: (Word256, Operation) -> [Label] -> HplOp O C
        TailOp :: [(Word256, Operation)] -> HplOp O C

instance Show (HplOp e x)

instance Eq (HplOp C O) where
  (==) (CoOp a) (CoOp b) = a == b

instance Eq (HplOp O O) where
  (==) (OoOp a) (OoOp b) = a == b

instance Eq (HplOp O C) where
  (==) (TailOp a) (TailOp b) = a == b
  (==) (OcOp a _) (OcOp b _) = a == b
  (==) _ _ = False

instance NonLocal HplOp where
  entryLabel (CoOp l) = l
  successors (OcOp _ ll) = ll
  successors (TailOp _) = []

type HplBody = Body HplOp

-- evmOp2HplOp :: (Word256, Operation) -> WordLabelMapM (Either (HplOp O O) (HplOp O C))
-- evmOp2HplOp op@(loc, STOP) = return $ Right $ OcOp op []
-- evmOp2HplOp op = error ("Unimplemented(evmOp2HplOp):" ++ show op)
evmOps2HplBody :: [(Word256, Operation)] -> WordLabelMapM HplBody
evmOps2HplBody [] = return emptyBody
evmOps2HplBody el@((loc, _):t) = do
  l <- labelFor loc
  addBlock (blockJoin (CoOp l) emptyBlock (TailOp el)) <$> evmOps2HplBody t

--------------------------------------------------------------------------------
-- The WordLabelMapM monad
--------------------------------------------------------------------------------
type WordLabelMap = Bimap Word256 Label

data WordLabelMapM a =
  WordLabelMapM (WordLabelMap -> SimpleUniqueMonad (WordLabelMap, a))

labelFor :: Word256 -> WordLabelMapM Label
labelFor word = WordLabelMapM f
  where
    f m =
      case DB.lookup word m of
        Just l' -> return (m, l')
        Nothing -> do
          l' <- freshLabel
          let m' = DB.insert word l' m
          return (m', l')

labelsFor :: [Word256] -> WordLabelMapM [Label]
labelsFor = mapM labelFor

instance Monad WordLabelMapM where
  return = pure
  WordLabelMapM f1 >>= k =
    WordLabelMapM $
    \m -> do
      (m', x) <- f1 m
      let (WordLabelMapM f2) = k x
      f2 m'

instance Functor WordLabelMapM where
  fmap = liftM

instance Applicative WordLabelMapM where
  pure x = WordLabelMapM (\m -> return (m, x))
  (<*>) = ap