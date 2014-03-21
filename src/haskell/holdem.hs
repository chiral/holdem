------------------------------------------------------------
-- "Text Holdem" -- Texas Holdem Program in Haskell
------------------------------------------------------------
module Main where

import Data.Maybe
import Data.List as L
import Data.IntMap (IntMap)
import Data.IntMap ((!))
import qualified Data.IntMap as M
import Data.Char

import Control.Monad
import Control.Monad.State

import System.Random

type IM = M.IntMap

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

type Player = Int -- 0 - (NumSeats-1)
type Score = Int
type Blind = [Score] -- SB,BB
type Ante = Score

data PlayerState = Away | Folded | Active | Allin
                 deriving (Show,Enum,Eq)

data Table = Table { 
      scores :: IM Score,
      btn :: Player,
      ante :: Ante,
      blind :: Blind
    } deriving (Show)

data GameState = GameState { 
      players :: IM PlayerState, 
      hand :: IM Cards,
      deck :: Cards,
      board :: Cards,
      pot :: IM Score,
      minBet :: Score,
      current :: Player
    } deriving (Show)

type GS = GameState
type TS = StateT Table IO

initAnte = 0
initBlind = [5,10]
initScore = 1000

initTable :: Int -> Table
initTable np = 
    Table { 
      scores=M.fromList $ zip [0..] $ L.replicate np initScore,
      btn=0,
      ante=initAnte,
      blind=initBlind
    }

takes :: Int -> Int -> [a] -> [[a]]
takes 0 _ lis = []
takes n m lis = take m lis:takes (n-1) m (drop m lis)

initGameState :: Cards -> TS GS
initGameState cards = do
    t <- get
    let ss = scores t
        n = M.size ss
        onSeats = map (\i->(i,(ss!i)>0)) [0..n-1]
        conv f (p,b) x = if b then (p,x) else (p,f x)
        forPlayers f = M.fromList . zipWith (conv f) onSeats
    return GameState { 
      players=forPlayers (const Away) $ repeat Active,
      hand=forPlayers id $ takes n 2 cards,
      deck=drop (2*n) cards,
      board=[],
      pot=forPlayers id $ repeat 0,
      minBet=0, 
      current=btn t
    }

doBet :: Player -> Score -> GS -> TS GS
doBet p s gs = do
  t <- get
  let score = scores t ! p
      bet = min s score
      allin x = if bet==score then Allin else x
  put t { scores=M.adjust (+(-bet)) p $ scores t }
  return gs { players=M.adjust allin p $ players gs,
              pot=M.adjust (+bet) p $ pot gs,
              minBet=s }

type Bets = [(Player,Score)]

doBets :: Bets -> GS -> TS GS
doBets bets gs = foldM (flip ($)) gs bet_actions
  where bet_actions = map (uncurry doBet) bets

doAnteBets :: GS -> TS GS
doAnteBets gs = do
  t <- get
  let ps = players gs
      active = L.filter ((==Active).(ps!)) $ M.keys ps
  flip doBets gs $ zip active (repeat $ ante t)

first :: (a->Bool) -> [a] -> a
first f = head . (L.filter) f 

nextPlayers :: GS -> [Player]
nextPlayers gs = a++b
    where 
      ps = players gs
      n = M.size ps
      next = (current gs+1) `mod` n
      (a,b) = splitAt next [0..n-1]

nextPlayer :: GS -> Player
nextPlayer gs = first ((==Active).(ps!)) lis
    where 
      ps = players gs
      lis = nextPlayers gs
                
io :: IO a -> TS a
io = liftIO

printBoard :: GS -> Table -> IO ()
printBoard g t = do
  putStrLn $ "scores: " ++ show (M.toList $ scores t)
  putStrLn $ "board: " ++ show (board g)
  putStrLn $ "pot: " ++ show (M.foldl (+) 0 $ pot g)
  putStrLn $ "player: " ++ show (nextPlayer g)

getBet :: GS -> TS Score
getBet g = do
    let mb = minBet g
    io $ putStrLn $ "minimum bet = " ++ show mb
    io $ putStrLn "your action? [(f)old or (c)all or (b)et/raise]"
    line <- io $ getLine
    case (head line) of
      'f' -> return 0
      'c' -> return mb
      'b' -> do
        io $ putStrLn "bet amount? "
        line1 <- io $ getLine
        return (read line1 :: Int)

doBetAround :: GS -> TS GS
doBetAround g = do
  t <- get
  io $ printBoard g t
  bet <- getBet g
  doBet (nextPlayer g) bet g

doPreflop :: GS -> TS GS
doPreflop gs = do
  gs1 <- doAnteBets gs
  t <- get
  let [sb,bb] = blind t
  gs2 <- doBet (nextPlayer gs1) sb gs1
  gs3 <- doBet (nextPlayer gs2) bb gs2
  doBetAround gs3

openOne :: GS -> GS
openOne gs =
  let (c1:d1) = deck gs
  in gs {deck=d1,board=board gs++[c1]}

doFlop :: GS -> TS GS
doFlop = doBetAround.openOne.openOne.openOne

doTurn :: GS -> TS GS
doTurn = doBetAround.openOne

doRiver :: GS -> TS GS
doRiver = doBetAround.openOne

doGame :: Cards -> TS GS
doGame cards = do
  g <- initGameState cards
  foldM (flip ($)) g [doPreflop,doFlop,doTurn,doRiver]

-- Main 

main = do
  g <- getStdGen
  putStrLn "num player? [2-10]"
  line <- getLine
  let np = read line
  repeatGames (randoms g) (initTable np)

repeatGames :: RandInts -> Table -> IO ()
repeatGames rs t = do
  let (rs1,rs2) = splitAt 52 rs
      cards = shuffle rs1 initCards
  execStateT (doGame cards) t >>= repeatGames rs2
  
