-- import List
import Numeric
import Control.Monad
-- import System
-- import Text.XHtml.Strict
import Data.Unique
import Data.Time


data Currency = USD | GBP | EUR | ZAR | KYD | CHF  
                deriving (Eq, Show)

data Equity = IBM | DEL 
                deriving (Eq, Show)

type Date = (CalendarTime, TimeStep)
type TimeStep = Int
type CalendarTime = ()

mkDate :: TimeStep -> Date
mkDate s = ((),s)

time0 :: Date
time0 = mkDate 0


-- Observable data type -----
newtype Obs a = Obs (Date -> PR a)

instance Show a => Show (Obs a) where
   show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"



-- Contract primitives ------
data Contract = Zero
                | One  Currency
		        | OneE  Equity
                | Give Contract
                | And  Contract Contract
                | Or   Contract Contract
                | Cond    (Obs Bool)   Contract Contract
                | Scale   (Obs Double) Contract
                | When    (Obs Bool)   Contract
                | Anytime (Obs Bool)   Contract
                | Until   (Obs Bool)   Contract
                deriving Show


zero :: Contract
zero = Zero

one :: Currency -> Contract
one = One

give :: Contract -> Contract
give = Give

cAnd :: Contract -> Contract -> Contract
cAnd = And

cOr :: Contract -> Contract -> Contract
cOr = Or

cond :: Obs Bool -> Contract -> Contract -> Contract
cond = Cond

scale :: Obs Double -> Contract -> Contract
scale = Scale

cWhen :: Obs Bool -> Contract -> Contract
cWhen = When

anytime :: Obs Bool -> Contract -> Contract
anytime = Anytime

cUntil :: Obs Bool -> Contract -> Contract
cUntil = Until

-- Derived primitive -------------
andGive :: Contract -> Contract -> Contract
andGive c d = c `cAnd` give d



-- Primitives over observables ---
konst :: a -> Obs a
konst k = Obs (\t -> bigK k)

lift :: (a -> b) -> Obs a -> Obs b
lift f (Obs o) = Obs (\t -> PR $ map (map f) (unPr $ o t))

lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
lift2 f (Obs o1) (Obs o2) = Obs (\t -> PR $ zipWith (zipWith f) (unPr $ o1 t) (unPr $ o2 t))

date :: Obs Date
date = Obs (\t -> PR $ timeSlices [t])

instance Num a => Num (Obs a) where
                    fromInteger i = konst (fromInteger i)
                    (+) = lift2 (+)
                    (-) = lift2 (-)
                    (*) = lift2 (*)
                    abs = lift abs
                    signum = lift signum

instance Eq a => Eq (Obs a) where
                (==) = undefined

(==*) :: Ord a => Obs a -> Obs a -> Obs Bool
(==*) = lift2 (==)

at :: Date -> Obs Bool
at t = date ==* (konst t)

(%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
(%<)  = lift2 (<)
(%<=) = lift2 (<=)
(%=)  = lift2 (==)
(%>=) = lift2 (>=)
(%>)  = lift2 (>)



-- Option contracts --------------
european :: Date -> Contract -> Contract
european t u = cWhen (at t) (u `cOr` zero)

american :: (Date, Date) -> Contract -> Contract
american (t1, t2) u = anytime (between t1 t2) u

between :: Date -> Date -> Obs Bool
between t1 t2 = lift2 (&&) (date %>= (konst t1)) (date %<= (konst t2))



-- Value processes ---------------
newtype PR a = PR { unPr :: [RV a] } deriving Show
type RV a = [a]

takePr :: Int -> PR a -> PR a
takePr n (PR rvs) = PR $ take n rvs

horizonPr :: PR a -> Int
horizonPr (PR rvs) = length rvs

andPr :: PR Bool -> Bool
andPr (PR rvs) = and (map and rvs)



-- Model -------------------------
data Model = Model {
                modelStart :: Date,
                disc       :: Currency -> (PR Bool, PR Double) -> PR Double,
                exch       :: Currency -> Currency -> PR Double,
                absorb     :: Currency -> (PR Bool, PR Double) -> PR Double,
                rateModel  :: Currency -> PR Double
                }


exampleModel :: CalendarTime -> Model
exampleModel modelDate = Model {
                modelStart = (modelDate,0),
                disc       = disc,
                exch       = exch,
                absorb     = absorb,
                rateModel  = rateModel
                }
                where
                    -- Interest rate model
                    rates :: Double -> Double -> PR Double
                    rates rateNow delta = PR $ makeRateSlices rateNow 1
                        where
                            makeRateSlices rateNow n = (rateSlice rateNow n) : (makeRateSlices (rateNow-delta) (n+1))
                            rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]

                    rateModels = [(CHF, rates 7   0.8)
                                 ,(EUR, rates 6.5 0.25)
                                 ,(GBP, rates 8   0.5)
                                 ,(KYD, rates 11  1.2)
                                 ,(USD, rates 5   1)
                                 ,(ZAR, rates 15  1.5)]

                    rateModel k = case lookup k rateModels of
                                    Just x -> x
                                    Nothing -> error $ "rateModel: currency not found " ++ (show k)


                    -- Disc primitive
                    disc :: Currency -> (PR Bool, PR Double) -> PR Double
                    disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)

                    discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
                    discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
                        if and bRv -- test for horizon
                            then [pRv]
                            else
                                let
                                    rest@(nextSlice:_) = discCalc bs ps rs
                                    discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
                                    thisSlice = zipWith3 (\b p q -> if b then p else q) bRv pRv discSlice
                                in thisSlice : rest

                    prevSlice :: RV Double -> RV Double
                    prevSlice [] = []
                    prevSlice (_:[]) = []
                    prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest


                    -- Absorb primitive
                    absorb :: Currency -> (PR Bool, PR Double) -> PR Double
                    absorb k (PR bSlices, PR rvs) =
                                PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
                                            bSlices rvs


                    -- Exchange rate model
                    exch :: Currency -> Currency -> PR Double
                    exch k1 k2 = PR (konstSlices 1)


                    -- Expected value
                    expectedValue :: RV Double -> RV Double -> Double
                    expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

                    expectedValuePr :: PR Double -> [Double]
                    expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice


-- Probability calculation
probabilityLattice :: [RV Double]
probabilityLattice = probabilities pathCounts
    where
        probabilities :: [RV Integer] -> [RV Double]
        probabilities (sl:sls) = map (\n -> (fromInteger n) / (fromInteger (sum sl))) sl : probabilities sls

        pathCounts :: [RV Integer]
        pathCounts = paths [1] where paths sl = sl : (paths (zipWith (+) (sl++[0]) (0:sl)))


-- Compositional valuation semantics for contracts    
evalC :: Model -> Currency -> Contract -> PR Double
evalC (Model modelDate disc exch absorb rateModel) k = eval    -- punning on record fieldnames for conciseness
    where   eval Zero           = bigK 0
            eval (One k2)       = exch k k2
            eval (Give c)       = -(eval c)
            eval (o `Scale` c)  = (evalO o) * (eval c)
            eval (c1 `And` c2)  = (eval c1) + (eval c2)
            eval (c1 `Or` c2)   = max (eval c1) (eval c2)
            eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
            eval (When o c)     = disc   k (evalO o, eval c)
    --      eval (Anytime o c)  = snell  k (evalO o, eval c)
            eval (Until o c)    = absorb k (evalO o, eval c)


-- Valuation semantics for observables    
evalO :: Obs a -> PR a
evalO (Obs o) = o time0


-- Process primitives    
bigK :: a -> PR a
bigK x = PR (konstSlices x)

konstSlices x = nextSlice [x]
   where nextSlice sl = sl : (nextSlice (x:sl))

datePr :: PR Date
datePr = PR $ timeSlices [time0]

timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]

condPr :: PR Bool -> PR a -> PR a -> PR a
condPr = lift3Pr (\b tru fal -> if b then tru else fal)

liftPr :: (a -> b) -> PR a -> PR b
liftPr f (PR a) = PR $ map (map f) a

lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b

lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
zipWithAll f as@(_:_) []       = as
zipWithAll f []       bs@(_:_) = bs
zipWithAll _ _        _        = []


instance Num a => Num (PR a) where
    fromInteger i = bigK (fromInteger i)
    (+) = lift2PrAll (+)
    (-) = lift2PrAll (-)
    (*) = lift2PrAll (*)
    abs = liftPr  abs
    signum = liftPr signum

instance Ord a => Ord (PR a) where
    max = lift2Pr max

instance Eq a => Eq (PR a) where
    (PR a) == (PR b) = a == b






-- Examples -----------------------------------
xm :: Model
xm = exampleModel ()

evalX :: Contract -> PR Double
evalX = evalC xm USD

zcb :: Date -> Double -> Currency -> Contract
zcb t x k = cWhen (at t) (scale (konst x) (one k))

c1 :: Contract
c1 = zcb t1 10 USD

t1 :: Date
t1 = mkDate t1Horizon

t1Horizon = 3 :: TimeStep

c11 :: Contract
c11 = european (mkDate 2)
            (zcb (mkDate 20) 0.4 USD `cAnd`
             zcb (mkDate 30) 9.3 USD `cAnd`
             zcb (mkDate 40) 109.3 USD `cAnd`
             give (zcb (mkDate 12) 100 USD))


pr1 :: PR Double
pr1 = evalX c1

tr1 = unPr pr1

future:: Date -> Equity -> Double -> Double -> Currency -> Contract
future t equity quantity price currency = cWhen (at t) (scale (konst quantity) (OneE equity)) `cAnd` Give ((scale (konst price) (One currency)))

f1= future (mkDate 30) IBM 100 2000.0 USD

