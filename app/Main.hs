{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow.Assemble
import Control.Monad
import Control.Arrow
import Data.Typeable
import Data.Text
import Data.Maybe 

data A = A 
data B = B
data C = C
data D = D
data E = E
data F = F

f1 :: A -> IO B
f1 _ = print "A->B" >> return B

f2 :: A -> IO C
f2 _ = print "A->C" >> return C

f3 :: (B,C) -> IO D
f3 _ = print "(B,C)->D" >> return D

f4 :: D -> IO E
f4 _ = print "D->E" >> return E

f5 :: E -> IO F
f5 _ = print "E->F" >> return F

pieces = 
	[
		Piece "f1" (mkA $ Kleisli f1),
		Piece "f2" (mkA $ Kleisli f2),
		Piece "f3" (mkA $ Kleisli f3),
		Piece "f4" (mkA $ Kleisli f4),
		Piece "f5" (mkA $ Kleisli f5)
	]

main :: IO ()
main = do 
	let xs = runFiveYearOld pieces (typeRep (Proxy :: Proxy A)) (typeRep (Proxy :: Proxy F))
	case xs of 
		[]    -> print "no solutions"
		(x:_) -> fromMaybe (print "invalid") $ tryRunAF x

	where
		tryRunAF (Assembly (a :: Kleisli IO a b) _ _) = do 
			kl <- cast a :: Maybe (Kleisli IO A F)
			return $ (runKleisli kl A >> return ())