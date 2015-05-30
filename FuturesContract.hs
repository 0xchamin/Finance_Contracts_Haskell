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
-- ********************************************************************************************************

-- import List
import Numeric
import Control.Monad
-- import System
-- import Text.XHtml.Strict
import Data.Unique
import Data.Time
--import Control.Monad.when

data Currency = USD | GBP | LKR 
					deriving (Eq, Show, Ord)

data Brand = IBM | DELL | SAMSUNG
				    deriving (Eq, Show, Ord)

data MDate = MDate Int 
	deriving Show

-- Observable data type -----
newtype Obs a = Obs (MDate -> a)

instance Show a  =>  Show (Obs a) where
	show (Obs o) = "(Obs " ++ show  (o today) ++ ")"	 

today :: MDate 
today = MDate 0


data Contract = Zero 	|
				One Currency	|
				Give Contract 	|
				Scale (Obs Double) Contract 	|
				When (MDate) Contract 	|
				And Contract Contract
				deriving Show


-- **************************************

-- Define a combinator interface to the Contract datatype
 	
 -- [in contract we use combinators in simple letters -> to make  it sense for the language : we have to expose combinators to language data types via an interface] 

-- **************************************

one :: Currency -> Contract
one = One

scale :: Obs Double -> Contract -> Contract
scale = Scale

when :: MDate -> Contract -> Contract
when = When

cAnd :: Contract -> Contract -> Contract
cAnd = And 

{-
According to contract language the primitives over abservables, the constand defined as mehtod signature
When looking at "Obs a" definition, it says, taking date as an input parameter and returns time variying value. 
So we add "\t". 
-}

konst :: a -> Obs a
konst k = Obs (\t -> k)

{-

----------------************* Derived primitive ***************  -------------

-}


sharePrice ::  Obs Double -> Contract -> Contract
sharePrice od u = scale od u 

testing :: MDate ->  Double  -> Currency  -> Contract
testing t d  k = (When t) (Scale (konst d) (One k)) 


{-
payment t d  k = (when t) (scale (konst d) (one k)) 
----------------************* Payment of cash ***************  ------------ 
c1 :: Contract
c1 = sharePrice

ghci >future (MDate 2) (Equity IBM) 200 120 (Currency GBP)
And (When (Obs False) (Scale (Obs 200.0) (One (Equity IBM)))) (Give (When (Obs False) (Scale (Obs 120.0) (One (Currency GBP)))))


-}



{-

----------------************* Receiving of IBM sares ***************  -------------

----------------************* Payment of cash ***************  ------------

-}
------ ************** Defining Future Contract ************** -----------


{-
t - Exercise date
q - Quantity
b - brand
sp - sharePrice
-}

--furure :: MDate -> Double -> Brand -> sharePrice -> Contract
--sfurure t q b sp = when t 
 