module FuturesContract where

-- ******************************************************************************************************
--  Let's look at an example of going long. It's January and you enter into a futures contract to purchase 
--  100 shares of IBM stock at $50 a share on April 1. The contract has a price of $5,000. But if the market 
--  value of the stock goes up before April 1, you can sell the contract early for a profit. Let's say 
--  the price of IBM stock rises to $52 a share on March 1. If you sell the contract for 100 shares, you'll 
--  fetch a price of $5,200, and make a $200 profit.

{-
	Contract Characteristics

	Commodity -> IBM
	Date -> 1st of April
	Quantity -> 100
	Share Price -> 50

	from PJ -> When One  scale get
	
	share price =  scale (obs double) (one usd) // idea: share price is a time variying quantity
	
	represent IBM share price => IBM & Share Price // Brand can be either IBM | Samsung

	now represent share price of a particular brand

	EquityPrice :: Brand  -> sharePrice -> Contract

	application of list (to give the idea of a bulk of shares)

	here we have to model both receiving of cash and payment

-}

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
				Get Contract 	|
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

get :: Contract -> Contract
get = Get

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

--c2 = zcb (MDate 0) 200.0 (Currency LKR)

--c3 :: Contract
--c3 = Give c2

{-

**************************************** Receiving of Goods  ********************************************
goodsReceive = get (zcb (MDate 2) 200.0 (Equity IBM))

-}

goodsReceive :: MDate -> Double -> Transfer -> Contract
goodsReceive excerciseDate quantity commodity = get (zcb (excerciseDate) quantity (commodity))

{-

**************************************** Cash settlement  ********************************************
settlement = give (zcb (MDate 2) 200.0 (Currency LKR))

-}

settlement :: MDate -> Double -> Transfer -> Contract
settlement excerciseDate price currency = give (zcb (excerciseDate) price (currency))



{-

**************************************** Future's Contract  ********************************************
-}

future :: MDate -> Double -> Transfer -> Double -> Transfer -> Contract
future excerciseDate quantity commodity price currency = cAnd (goodsReceive excerciseDate quantity commodity) (settlement excerciseDate price currency) 
