module OptionsContract where

{-
Company XYZ's stock price to go up to $90 in the next month.  The trader sees that he can buy an options contract of Company XYZ
 at $4.50 with a strike price of $75 per share.  The trader must pay the cost of the option ($4.50 X 100 shares = $450).  
 The stock price begins to rise as expected and stabilizes at $100.  Prior to the expiry date on the options contract,
  the trader executes the call option and buys the 100 shares of Company XYZ at $75, the strike price on his options contract.  
  He pays $7,500 for the stock.  The trader can then sell his new stock on the market for $10,000, making a $2,050 profit ($2,500 minus $450 for the options contract)


-}

-- import List
import Numeric
import Control.Monad
-- import System
-- import Text.XHtml.Strict
import Data.Unique
import Data.Time

data Code = USD | LKR | GBP 
			deriving (Show, Eq, Ord)

data Symbol = IBM | SAMSUNG | DELL 
			deriving (Show, Eq, Ord)


data Transfer = Currency Code | Equity Symbol | Empty	
			deriving (Show, Eq, Ord)

data MDate = MDate Int

newtype Obs a = Obs(MDate -> a)

instance Show a => Show (Obs a) where
   show (Obs o) = "(Obs " ++ show  (o today) ++ ")"	 

today :: MDate
today = MDate 0  

data Contract = Zero	|
				One Transfer	|
				OneE Transfer	|
				Give Contract 	|
				Or 	Contract Contract 	|
				And Contract Contract 	|
				Then Contract Contract 	|
				Anytime Contract 	|
				Get Contract 	|
				Scale (Obs Double) Contract |
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

oneE  :: Transfer -> Contract
oneE = OneE

give :: Contract -> Contract
give = Give

cOr :: Contract -> Contract -> Contract
cOr = Or

cAnd :: Contract -> Contract -> Contract
cAnd = And

scale :: (Obs Double) -> Contract -> Contract
scale = Scale

get :: Contract -> Contract
get = Get

cWhen :: (Obs Bool) -> Contract -> Contract
cWhen = When

cThen :: Contract -> Contract -> Contract
cThen = Then 

cAnytime :: Contract -> Contract
cAnytime = Anytime 

{-
****************************** defining observables ****************************
-}

konst :: a -> Obs a 
konst k = Obs (\t -> k) {-go to Obs a definition. There we see a mapping from time to data time-}

sameDate :: MDate -> MDate -> Bool
sameDate (MDate t1) (MDate t2) = t1 == t2

at :: MDate -> Obs Bool
at future_t = Obs (\t -> sameDate future_t t)

{-
c1 :: Contract
c1 = give (One (Equity IBM))
-}

zcb :: MDate -> Double -> Transfer -> Contract
zcb t x k = cWhen (at t) (scale (konst x)(one k))

{-
********************************************* Receiving of Commodities ***********************************************
-}

goodsReceive :: MDate -> Double -> Transfer -> Contract
goodsReceive exerciseDate quantity commodity = get (zcb exerciseDate quantity commodity)

{-
********************************************* Settlement of Commodities ***********************************************
-}

settlment :: MDate -> Double -> Transfer -> Contract
settlment exerciseDate strikePrice currency = give (zcb exerciseDate strikePrice currency)

prefferedEuropean :: MDate -> Double -> Transfer -> Double -> Transfer -> Contract
prefferedEuropean exerciseDate quantity commodity strikePrice currency = cAnd (goodsReceive exerciseDate quantity commodity) (settlment exerciseDate strikePrice currency) 


{-
********************************************* European Option ***********************************************
-}
europeanOption :: MDate -> Double -> Transfer -> Double -> Transfer -> Contract
europeanOption exerciseDate quantity commodity strikePrice currency  = get ((prefferedEuropean exerciseDate quantity commodity strikePrice currency) `cOr` zero)

{-
********************************************* American Option ***********************************************
-}

americanOption :: MDate -> Double -> Transfer -> Double -> Transfer -> Contract
americanOption exerciseDate quantity commodity strikePrice currency  = cAnytime (get ((prefferedEuropean exerciseDate quantity commodity strikePrice currency) `cOr` zero))