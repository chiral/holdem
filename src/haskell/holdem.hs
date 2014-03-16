------------------------------------------------------------
-- "Text Holdem" -- Texas Holdem Program in Haskell
------------------------------------------------------------
module Main where

import Data.Maybe
import Data.List as L
import Data.Char

import Control.Monad
import Control.Monad.State

import System.Random

update :: Int -> a -> [a] -> [a]
update k v lis = take k lis ++ [v] ++ drop (k+1) lis

-- Cards

data Suit = S | H | D | C deriving (Eq,Enum,Show,Read)

newtype Rank = Rank Int deriving (Eq,Enum)
rankChar = "0123456789TJQK"

instance Show Rank where
    showsPrec _ = (:).(rankChar!!).fromEnum

instance Read Rank where
    readsPrec _ (x:xs) = maybeToList $ elemIndex x rankChar >>= return.flip (,) xs.toEnum

data Card = Card Rank Suit

instance Show Card where
    showsPrec _ (Card r s) = (++) $ show r ++ (L.map toLower $ show s)

instance Read Card where
    readsPrec _ t = [(Card r s,us) | (r,u:us)<-reads t,(s,_)<-reads [toUpper u]]

type Cards = [Card]

initCards = L.map (uncurry Card) [(toEnum r,toEnum s) | r<-[1..13],s<-[0..3]]

type RandInts = [Int]

shuffle :: RandInts -> [a] -> [a]
shuffle rs xs = snd.unzip $ L.sortBy (\x y->compare (fst x) (fst y)) $ L.zip rs xs

-- Game

type Player = Int -- 0 - (N-1)
type NumPlayers = Int
type BtnPos = (NumPlayers,Int)
type Score = Int
type Blind = (Score,Score)
type Ante = Score

data Stage = Preflop | Flop | Turn | River | ShowDown 
             deriving (Eq,Enum,Show)

data TableState = Table {
      btnPos :: BtnPos,
      ante :: Ante,
      blind :: Blind,
      scores :: [Score] -- player -> current score
    } deriving (Show)

type TS = TableState

initAnte = 0
initBlind = (5,10)
initScore = 1000

initTable :: NumPlayers -> TS
initTable np = Table { btnPos=(np,0),
                       ante=initAnte,
                       blind=initBlind,
                       scores=L.replicate np initScore }

nplayers = fst.btnPos

type AllIn = Bool

data Board = Board { 
      board :: Cards,
      deck :: Cards,
      hand :: [Cards], -- player -> cards in hand
      active :: [Player], 
      allin :: [Player], 
      bet :: Score,
      pot :: [Score] -- player -> amount bet in a game
    } deriving (Show)

takes :: Int -> Int -> [a] -> ([[a]],[a])
takes 0 _ lis = ([],lis)
takes n m lis = (a:fst r,snd r) 
    where (a,b)=splitAt m lis
          r = takes (n-1) m b

initBoard :: Cards -> State TS Board
initBoard cards = do
  ts <- get
  let (np,btn) = btnPos ts
      (hand1,deck1) = takes np 3 cards
      sb = (btn+1) `mod` np
      (a,b) = splitAt sb [0..np-1]
  return $ Board { 
               board=[],
               deck=deck1,
               hand=hand1,
               active=b++a,
               allin=[],
               bet=0,
               pot=replicate np 0 }

doBet :: Board -> Score -> State TS Board
doBet b s = do
  ts <- get
  let (p:active1) = active b
      bet1 = min s $ scores ts !! p
      s1 = (scores ts !! p) - bet1
      pot1 = (pot b !! p) + bet1
      (p1,a1) = if s1==0 then ([p],[]) else ([],[p])
  put $ ts { scores=update p s1 $ scores ts }
  return b { active=active1++p1,
             allin=allin b++a1, bet=s,
             pot=update p pot1 $ pot b }

doAnte :: Board -> State TS Board
doAnte b = do
  ts <- get
  let np = nplayers ts
  foldM doBet b $ replicate np $ ante ts

doBlind :: Board -> State TS Board
doBlind b = do
  ts <- get
  let (sb,bb) = blind ts
  foldM doBet b [sb,bb]

printState :: TS -> Board -> Player -> IO ()
printState ts b p = return ()

type GameState a = StateT TS IO a

preFlop :: Board -> GameState Board
preFlop b = do
  ts <- get
  lift $ printState ts b $ head $ active b
  return b

playGame :: TS -> Cards -> IO TS
playGame ts cards = do
  print cards
  putStrLn "your action. [(c)all or (f)old or (r)aise]"
  line <- getLine
  return ts

-- Main 

main = do
  g <- getStdGen
  putStrLn "num player? [2-10]"
  line <- getLine
  let np = read line
  repeatGames (initTable np) (randoms g)

repeatGames :: TS -> RandInts -> IO ()
repeatGames ts rs = do
  let (rs1,rs2) = splitAt 52 rs
      cards = shuffle rs1 initCards
  ts1 <- playGame ts cards
  let np1 = nplayers ts1
      win = showWin ts1
      next = repeatGames ts1 rs2
  if np1==1 then win else next

showWin :: TS -> IO ()
showWin ts = do
  let Just p = findIndex (>0) $ scores ts
  print $ "player " ++ show (p+1) ++ " win!"
