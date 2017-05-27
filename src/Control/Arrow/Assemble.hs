{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Control.Arrow.Assemble where


import Control.Arrow 
import Control.Monad 
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Logic

import qualified Data.Text as T
import Data.Text(Text)
import Data.Maybe
import Data.Typeable
import Data.Typeable.Internal

import Debug.Trace



-- | A wrapped arrow with type hints
data Assembly a where
  Assembly :: (Arrow a, Typeable a, Typeable b, Typeable c)
    => a b c -> TypeRep -> TypeRep -> Assembly a

-- | wrap an typeable arrow (handle the Proxy stuff)
mkA :: forall a b c . 
  (
    Arrow a, 
    Typeable a, 
    Typeable b, 
    Typeable c
  ) => a b c -> Assembly a
mkA x = Assembly x (typeRep (Proxy :: Proxy b)) (typeRep (Proxy :: Proxy c))

listToLogic :: [a] -> LogicT m a
listToLogic xs = LogicT (\f x0 -> foldr f x0 xs)

maybeToLogic :: Maybe a -> LogicT m a
maybeToLogic = listToLogic . maybeToList

startsWithType :: Assembly a -> TypeRep -> Bool
startsWithType (Assembly _ tr1 _) tr = tr == tr1

endsWithType :: Assembly a -> TypeRep -> Bool
endsWithType (Assembly _ _ tr2) tr = tr == tr2

-- | A labeledarrow
data Piece a = Piece Text (Assembly a)

instance Show (Piece a) where
  show (Piece lbl _) = T.unpack lbl

instance Eq (Piece a) where
  (==) (Piece x _) (Piece y _) = x == y

removePiece :: Piece a -> [Piece a] -> [Piece a]
removePiece x = filter (/= x)

getTup :: TypeRep -> Maybe (TypeRep, TypeRep)
getTup (TypeRep _ tycon [] [t1,t2]) = do
    guard $ tycon == typeRepTyCon (typeRep (Proxy :: Proxy ((), ())))
    return (t1, t2)
getTup _ = Nothing


toSeq :: forall a . Assembly a -> Assembly a -> Maybe (Assembly a)
toSeq (Assembly (x :: a b c) _ _ )  (Assembly (y :: a c' d) _ _) = do
    x' <- cast x :: Maybe (a b c)
    y' <- cast y :: Maybe (a c d)
    return $ mkA (x' >>> y') 

--  (typeRep (Proxy :: Proxy b)) 
-- (typeRep (Proxy :: Proxy d))

toPar :: forall a . Assembly a -> Assembly a -> Maybe (Assembly a)
toPar (Assembly (a :: a b c) _ _)  (Assembly (b :: a b' c') _ _) = 
  return $ mkA (a *** b) 

-- (typeRep (Proxy :: Proxy (b,b'))) 
--   (typeRep (Proxy :: Proxy (c,c')))

toFan :: forall a . Assembly a -> Assembly a -> Maybe (Assembly a)
toFan (Assembly (a :: a b c) _ _) (Assembly (b :: a b' c') _ _) = do
  b' <- cast b :: Maybe (a b c')
  return $ mkA (a &&& b') 

--  (typeRep (Proxy :: Proxy b)) (typeRep (Proxy :: Proxy (c,c')))

-- | Use the given pieces to build an arrow with the given inputs/outpus
runFiveYearOld :: [Piece a] -> TypeRep -> TypeRep -> [Assembly a]
runFiveYearOld pieces tr1 tr2 = 
  fmap fst $ observeAll 
           $ runStateT m pieces
  where
    m = do 
      x <- fiveYearOld tr1      
      guard $ endsWithType x tr2
      return x
  
-- | Create an arrow with a given input type signature
fiveYearOld :: forall a . TypeRep -> StateT [Piece a] Logic (Assembly a)
fiveYearOld tr = do
    a   <- singlePiece `mplus` multiPiece
    more a `mplus` return a

  where
    singlePiece :: StateT [Piece a] Logic (Assembly a)
    singlePiece = do 
      pieces <- get
      piece@(Piece _ p) <- lift $ listToLogic pieces
      guard $ startsWithType p tr
      modify (removePiece piece)
      return p

    multiPiece :: StateT [Piece a] Logic (Assembly a)
    multiPiece = par `mplus` fan
      where
        par = do 
          (tr1, tr2) <- lift $ maybeToLogic $ getTup tr
          a' <- fiveYearOld tr1 
          b' <- fiveYearOld tr2
          maybe mzero return $ toPar a' b'      

        fan = do 
          a' <- singlePiece
          b' <- fiveYearOld tr
          (maybe mzero return $ toFan a' b') `mplus` (maybe mzero return $ toFan b' a')

    more :: Assembly a -> StateT [Piece a] Logic (Assembly a)
    more x@(Assembly arrow trA trB) = do 
      y <- fiveYearOld trB
      maybe mzero return $ toSeq x y



-- assembleNext :: forall a b . 
--     [ArrowAtom a b]
--   -> TypeRep
--   -> StateT (ArrowStruct a) (LogicT (Reader (b, AssembleDirection))) (WrapA a)
-- assembleNext atoms tr = do 

--     a <- basic `mplus` compound
--     out <- more a `mplus` nomore a
--     struct <- get
--     guard $ all (`isFinished` struct) atoms
--     return out


--   where

--     nomore a = return a

--     more :: WrapA a -> StateT (ArrowStruct a) (LogicT (Reader (b, AssembleDirection))) (WrapA a)
--     more x@(WrapA arrow trA trB) = do 

--       (_, dir) <- ask

--       let tr2 = if dir == LeftToRight then trB else trA

--       struct <- get
--       put (ArrowStruct (x:(parallel struct ++ split struct ++ previous struct)) [] [])
--       y <- assembleNext atoms tr2 
--       put struct

--       maybe mzero return $ if dir == LeftToRight then toSeq x y else toSeq y x

--     basic :: StateT (ArrowStruct a) (LogicT (Reader (b, AssembleDirection))) (WrapA a)
--     basic = do 
--       atom <- lift $ listToLogic atoms

--       (input, dir)  <- ask 
--       struct <- get
--       arrow <- lift $ maybeToLogic $ (toArrow atom) input struct

--       guard $ (if dir == LeftToRight then startsWithType else endsWithType) arrow tr
--       return arrow

--     compound :: StateT (ArrowStruct a) (LogicT (Reader (b, AssembleDirection))) (WrapA a)
--     compound = par `mplus` spl

--     par = do 
--       (tr1, tr2) <- lift $ maybeToLogic $ getTup tr
--       runBoth tr1 tr2 -- `mplus` runFst tr1 tr2 `mplus` runSnd tr1 tr2

--       where
--         runBoth tr1 tr2 = do
--           a' <- assembleNext atoms tr1 
--           modify (\s -> s{parallel = a' : parallel s})

--           b' <- assembleNext atoms tr2
--           modify (\s -> s{parallel = b' : parallel s})

--           maybe mzero return $ toPar a' b'

--     spl = do
--       (trL, trR) <- lift $ maybeToLogic $ getEither tr
--       runBoth trL trR -- `mplus` runLeft trL trR `mplus` runRight trL trR 

--       where
--         runBoth trL trR = do
--           a' <- assembleNext atoms trL 
--           modify (\s -> s{split = a' : split s})

--           b' <- assembleNext atoms trR
--           modify (\s -> s{split = b' : split s})

--           maybe mzero return (toSpl a' b')



-- getTup :: TypeRep -> Maybe (TypeRep, TypeRep)
-- getTup (TypeRep _ tycon [] [t1,t2]) = do
--     guard $ tycon == typeRepTyCon (typeRep (Proxy :: Proxy ((), ())))
--     return (t1, t2)
-- getTup _ = Nothing


-- getEither :: TypeRep -> Maybe (TypeRep, TypeRep)
-- getEither (TypeRep _ tycon [] [t1,t2]) = do
--     guard $ tycon == typeRepTyCon (typeRep (Proxy :: Proxy (Either () ())))
--     return (t1, t2)
-- getEither _ = Nothing





-- data PNum = PNum Text
-- data MtrNum = MtrNum Text
-- data Mtr = Mtr {
--   pnum :: PNum,
--   mtrnum :: MtrNum
-- }

-- testAtoms :: [ArrowAtom TmpA ()]
-- testAtoms = [
--     pnumFromUnit, 
--     mtrnumFromUnit,
--     pnumFromMtrNum,
--     mtrFromUnit
--   ]


-- toAtom :: (w :>+: LabelAn Text) => Text -> WrapA (SemanticArrow w eff) -> ArrowAtom (SemanticArrow w eff) ()
-- toAtom txt x = uniqueAtom txt $ ArrowAtom (const True) (\_ _ -> return x) (const True)

-- uniqueAtom :: 
--   (
--     w :>+: LabelAn c, 
--     Eq c
--   )
--   => c
--   -> ArrowAtom (SemanticArrow w eff) b 
--   -> ArrowAtom (SemanticArrow w eff) b 
-- uniqueAtom lbl (ArrowAtom chk f fin) = ArrowAtom chk f' fin
--   where
--     f' input str = do 
--       guard $ not $ isPresentStruct lbl str
--       (WrapA arrow tr1 tr2) <- f input str
--       return (WrapA (arrow @@ lbl) tr1 tr2)


-- notFstAtom ::
--      ArrowAtom (SemanticArrow w eff) b
--   -> ArrowAtom (SemanticArrow w eff) b
-- notFstAtom (ArrowAtom chk f fin) = ArrowAtom chk f' fin
--   where
--     f' input str = do
--       guard $ not $ isEmpty str
--       f input str
    
-- -- | testing
-- type TmpA = SemanticArrow (LabelAn Text) Null2

-- pnumFromUnit :: ArrowAtom TmpA ()
-- pnumFromUnit = uniqueAtom ("PNumFrom()" :: Text) $ ArrowAtom (const True) f (const True) 
--   where
--     f input str = return $ mkA (pur ((\_ -> PNum "P123") :: () -> PNum))

-- mtrnumFromUnit :: ArrowAtom TmpA ()
-- mtrnumFromUnit = uniqueAtom ("MtrNumFrom()" :: Text) $ ArrowAtom (const True) f (const True) 
--   where
--     f input str = return $ mkA (pur ((\_ -> MtrNum "456") :: () -> MtrNum))


-- pnumFromMtrNum :: ArrowAtom TmpA ()
-- pnumFromMtrNum = uniqueAtom ("PNumFromMtr" :: Text) $ ArrowAtom (const True) f (const True) 
--   where
--     f input str = return $ wrA (pur pnum)

-- mtrFromUnit :: ArrowAtom TmpA ()
-- mtrFromUnit = uniqueAtom ("MtrFromUnit" :: Text) $ ArrowAtom (const True) f (const True) 
--   where
--     f input str = return $ wrA (pur ((\_ -> Mtr (PNum "P789") (MtrNum "ABC")) :: () -> Mtr))



-- data PatentKw a b where
--   MtrNumKw :: PatentKw () MtrNum
--   PNumKw   :: PatentKw () PNum


-- data NoOptions = NoOptions deriving (Show, Typeable, Eq)

-- cmplAssemble :: forall w eff a b err . 
--   (
--     Annotation w,
--     eff :>+: Ex.ExceptionX err,

--     Typeable a,
--     Typeable b,
--     Typeable w,
--     Typeable eff,
--     Typeable err,
--     ?atticusError :: err,
--     err :>|: NoOptions
--   )
--   => [ArrowAtom (SemanticArrow w eff) ()] 
--   -> Proxy a 
--   -> Proxy b 
--   -> SemanticArrow w eff a b
-- cmplAssemble atoms p1 p2 = 
--     optionsA (lft NoOptions) opts >>> lftEff Ex.mustE
--   where
--     opts = mapMaybe toOption $ assembleFull atoms (typeRep p1) (typeRep p2) RightToLeft ()

--     toOption :: WrapA (SemanticArrow w eff) -> Maybe (SemanticArrow w eff a (Either err b))
--     toOption (WrapA x _ _) = do 
--       arrow <- cast x :: Maybe (SemanticArrow w eff a b)
--       return $ Ex.rwFtoE' arrow 

