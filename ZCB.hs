module ZCB where

	{-
ZCB = Receive $100 on 31st of Dec 2015

Contract properties
Currency : $
Amout : 100
Action : Receive
Date : 31st Dec 2015

	-}

import Numeric
import Control.Monad
-- import System
-- import Text.XHtml.Strict
import Data.Unique
import Data.Time

data Currency =  	USD 
				|	GBP
				|	LKR
				deriving (Show,Eq,Ord)


data Date = Date Int
			deriving (Show,Eq,Ord)

newtype Obs a = Obs (Date -> a) -- (Date -> PR a) | my frst attempt Date -> Double --

today :: Date 
today =  Date 0

instance Show a  =>  Show (Obs a) where
	show (Obs o) = "(Obs " ++ show  (o today) ++ ")"	

data Contract = 	Zero
				|	One Currency
				|	Give Contract
				|	And Contract Contract
				|	Or  Contract Contract
				| 	Scale (Obs Double) Contract
				|	When (Date) Contract
				deriving  (Show)



-- Define a combinator interface to the Contract datatype

{-- [in contract we use combinators in simple letters -> to make  it sense for the language :
	 we have to expose combinators to language data types via an interface] -}

one :: Currency -> Contract
one = One

scale :: (Obs Double) -> Contract -> Contract
scale = Scale

cWhen :: (Date) -> Contract -> Contract
cWhen = When


-- ***************** Defining Observables ***********************---------

konst:: a -> Obs a
konst a = Obs (\t ->  a)



{-Write down the symbolic representation of ZCB- ZCB :: this must be in combinators introduced by simon payton jones
	zcb :: cWhen t (scale d (one k) )

	instructions 
	1. first write combinators in "lowercase"
	2. then create data types for "Contracts & Observables"

	since zcb is not a combinator --> it takes parameters in the type signature
-}

zcb :: Date -> Double -> Currency -> Contract
zcb t d k = cWhen t (scale (konst d) (one k))


 
