module ZCB where

-- import List
import Numeric
import Control.Monad
-- import System
-- import Text.XHtml.Strict
import Data.Unique
import Data.Time

data Code = USD | GBP | LKR 
	deriving (Eq, Show, Ord)

data Symbol = IBM | SAMSUNG | DELL
	deriving (Eq, Show, Ord)

data Transfer = Currency Code | Equity Symbol | Null
	deriving (Eq, Show, Ord)


data MDate = MDate  Int
	deriving Show

newtype Obs a = Obs (MDate -> a)

instance Show a => Show (Obs a) where
   show (Obs o) = "(Obs " ++ show  (o today) ++ ")"	 

today :: MDate
today = MDate 0 

data Contract = Zero 	|	
				One Transfer	|	
				Give Contract 	|	
				Or 	Contract Contract 	|
				And Contract Contract 	|
				Scale (Obs Double) 	Contract	|
				When (Obs Bool) Contract 	
			deriving Show


-- **************************************

-- Define a combinator interface to the Contract datatype
 	
 -- [in contract we use combinators in simple letters -> to make  it sense for the language : we have to expose combinators to language data types via an interface] 

-- **************************************

zero :: Contract
zero = Zero

one :: Transfer -> Contract
one = One

give :: Contract -> Contract
give = Give

cOr :: Contract -> Contract -> Contract
cOr = Or

cAnd :: Contract -> Contract -> Contract
cAnd = And

scale :: (Obs Double) -> Contract -> Contract
scale = Scale

cWhen :: Obs Bool -> Contract -> Contract
cWhen = When

{-
********************** Defining Observables *************************
-}

konst :: a -> Obs a
konst k = Obs (\t -> k)

sameDate :: MDate -> MDate -> Bool
sameDate (MDate d1) (MDate d2) = (d1 == d2)

at :: MDate -> Obs Bool
at t_future = Obs (\t ->  sameDate t_future t)

{-

**************************************** Defining Zero Coupon Bond  ********************************************
-}

zcb :: MDate -> Double -> Transfer -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))

c2 = zcb (MDate 0) 200.0 (Currency LKR)

c3 :: Contract
c3 = Give c2




