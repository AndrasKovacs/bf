{-# language
  OverloadedStrings, LambdaCase, BangPatterns,
  PatternSynonyms, ViewPatterns, UnboxedTuples,
  ScopedTypeVariables #-}

import Control.Arrow
import Control.Applicative
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import System.Environment
import System.Exit
import Text.Show

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as IM

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

import Debug.Trace

-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  !inp <- getArgs >>= \case
    path:[] -> B.readFile path
    _       -> putStrLn "usage: fastbf PATH" >> exitSuccess
  run $ linearize $ parse inp

runTest = run . linearize .  parse
printTest = mapM_ print . zip [0..] . map showOp. linearize . parse

-- Opcodes
--------------------------------------------------------------------------------

-- 32 bit code layout, from least to most significant bits:
--   3   bits  : opcode      (positive)
--   8   bits  : operand arg (positive)
--   21  bits  : offset  arg (possibly negative)

type Op = Int32

pack :: Int32 -> Int -> Word8 -> Op
pack op offset arg =
      op
  .|. (unsafeShiftL (fromIntegral arg) 3)
  .|. unsafeShiftL (fromIntegral offset) 11

unpack1 :: Op -> Int32
unpack1 op = op .&. 7

unpack2 :: Op -> (Int32, Int)
unpack2 op = (op .&. 7, fromIntegral (unsafeShiftR op 11))

unpack3 :: Op -> (Int32, Int, Word8)
unpack3 op = (
  op .&. 7,
  fromIntegral (unsafeShiftR op 11),
  fromIntegral (unsafeShiftR op 3 .&. 255))

-- Need to pack the jumps differently because of sign extension
packJ :: Int32 -> Int -> Int -> Op
packJ op offset arg =
    op .|. (unsafeShiftL (255 .&. fromIntegral arg) 3)
    .|. unsafeShiftL (fromIntegral offset) 11

unpackJ :: Op -> (Int32, Int, Int)
unpackJ op = (
  op .&. 7,
  fromIntegral (unsafeShiftR op 11),
  fromIntegral (unsafeShiftR (unsafeShiftL op 21) 24))      

pattern Add o a    <- (unpack3 -> (0 , o, a)) where Add o a    = pack 0 o a
pattern Assign o a <- (unpack3 -> (1 , o, a)) where Assign o a = pack 1 o a
pattern Get o      <- (unpack2 -> (2 , o)   ) where Get o      = pack 2 o 0
pattern Put o      <- (unpack2 -> (3 , o)   ) where Put o      = pack 3 o 0

pattern MovJZ  o a <- (unpackJ -> (4 , o, a)) where MovJZ o a  = packJ 4 o a
pattern MovJNZ o a <- (unpackJ -> (5 , o, a)) where MovJNZ o a = packJ 5 o a
pattern Halt       <- (unpack1 -> 6         ) where Halt       = pack 6 0 0

-- | Intermediate instruction
pattern Mov o <- (unpack2 -> (7, o::Int)) where Mov o = pack 7 o 0

isCtrl :: Op -> Bool
isCtrl op = (op .&. 7) > 3

showOp :: Op -> String
showOp = \case
  Add o a    -> "Add " ++ show o ++ " " ++ show a
  Assign o a -> "Assign " ++ show o ++ " " ++ show a
  MovJZ o a  -> "MovJZ " ++ show o ++ " " ++ show a
  MovJNZ o a -> "MovJNZ " ++ show o ++ " " ++ show a
  Mov o      -> "Mov " ++ show o
  Get o      -> "Get " ++ show o
  Put o      -> "Put " ++ show o
  Halt       -> "Halt"
  _          -> error "showOp: invalid opcode"

-- AST
--------------------------------------------------------------------------------

data Block
  = Basic [Op]
  | While [Block]

instance Show Block where
  showsPrec n (Basic ops) =
    showParen (n > 10) (("Basic "++) . showListWith ((++).showOp) ops)
  showsPrec n (While bs) =
    showParen (n > 10) (("While "++) . showList bs)

-- Parsing
--------------------------------------------------------------------------------

pBasic :: P.Parser [Op]
pBasic = P.many1' $
      Add 0 1    <$ P.char '+'
  <|> Add 0 (-1) <$ P.char '-'
  <|> Mov 1      <$ P.char '>'
  <|> Mov (-1)   <$ P.char '<'
  <|> Get 0      <$ P.char ','
  <|> Put 0      <$ P.char '.'
  <|> Assign 0 0 <$ P.string "[-]"
  <|> Assign 0 0 <$ P.string "[+]"  

pBlocks :: P.Parser [Block]
pBlocks = P.many' $
      Basic <$> pBasic
  <|> While <$> (P.char '[' *> pBlocks <* P.char ']')

parse :: B.ByteString -> [Block]
parse = either (error "Parse error") id
      . P.parseOnly (pBlocks <* P.endOfInput)
      . B.filter (P.inClass "+-<>,.[]")

-- Coalescing
--------------------------------------------------------------------------------

coalesceBlock :: [Op] -> ([Op], Int)
coalesceBlock = first merge . offsets 0 where

  offsets o = \case
    Mov o'     : ops -> offsets (o + o') ops
    Add _ a    : ops -> first (Add o a    :) $ offsets o ops
    Get _      : ops -> first (Get o      :) $ offsets o ops
    Put _      : ops -> first (Put o      :) $ offsets o ops
    Assign _ v : ops -> first (Assign o v :) $ offsets o ops
    other      : ops -> first (other      :) $ offsets o ops
    []               -> ([], o)

  merge = \case
    Add _ 0 : ops                           -> merge ops
    Add o1 a    : Add o2 b : ops | o1 == o2 -> merge (Add o1 (a + b) : ops)
    Assign o1 a : Add o2 b : ops | o1 == o2 -> merge (Assign o1 (a + b) : ops)
    Add o1 a : Assign o2 b : ops | o1 == o2 -> merge (Assign o2 b : ops)
    other                  : ops            -> other : merge ops
    []                                      -> []

-- Create linear code
--------------------------------------------------------------------------------

linearize :: [Block] -> [Op]
linearize = flatten where

  -- Unfold nested blocks into a flat list, adding jump instructions
  flatten :: [Block] -> [Op]
  flatten blocks = evalState (fst <$> go 0 blocks) 0 [Halt] where
    
    go :: Int -> [Block] -> State Int ([Op] -> [Op], Int)
    go _ (Basic ops:bs) = do
      let (ops', o) = coalesceBlock ops
      modify (+(fromIntegral $ length ops'))
      first ((ops'++).) <$> go o bs    
    go o (While bs:bss) = do
      open      <- get <* modify (+1)
      (bs', o') <- go 0 bs
      close     <- get <* modify (+1)
      first (((MovJZ (close + 1) o:) . bs' . (MovJNZ (open + 1) o':)).) <$> go 0 bss
    go o [] = pure (id, o)

-- Interpretation
--------------------------------------------------------------------------------

memSize :: Int
memSize = 30000

run :: [Op] -> IO ()
run ops = do
  !(code :: Ptr Op)    <- newArray ops
  !(dat  :: Ptr Word8) <- callocArray memSize

  let go :: Ptr Op -> Ptr Word8 -> IO ()
      go !ip !i = do
        -- peekDat <- peekArray 10 dat        
        !op <- peek ip
        -- traceShowM (showOp op, minusPtr ip code, minusPtr i dat, peekDat)
        -- threadDelay 100000
        if isCtrl op then
          case op of
            MovJZ o a -> do
              let !i' = advancePtr i a
              peek i' >>= \case
                0 -> go (advancePtr code o) i'
                _ -> go (advancePtr ip 1) i'
            MovJNZ o a -> do
              let !i' = advancePtr i a              
              peek i' >>= \case
                0 -> go (advancePtr ip 1) i'
                _ -> go (advancePtr code o) i'
            Halt -> do
              pure ()
        else do
          case op of
            Add o a -> do
              let i' = advancePtr i o
              !val <- peek i'
              poke i' (val + a)
            Assign o a -> do
              poke (advancePtr i o) a
            Get o -> do
              c <- getChar
              poke (advancePtr i o) (fromIntegral (ord c))
            Put o -> do
              b <- peek (advancePtr i o)
              putChar (chr (fromIntegral b))
          go (advancePtr ip 1) i

  go code dat
  free code
  free dat



hello :: B.ByteString
hello = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

hello2 :: B.ByteString
hello2 = ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+."
