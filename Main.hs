{-# language
  OverloadedStrings, LambdaCase, BangPatterns,
  PatternSynonyms, ViewPatterns, UnboxedTuples,
  ScopedTypeVariables, NoMonomorphismRestriction #-}

import Control.Applicative
import Control.DeepSeq
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
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  !inp <- getArgs >>= \case
    path:[] -> B.readFile path
    _       -> putStrLn "usage: fastbf PATH" >> exitSuccess
  run $ compile $ inp

-- Opcodes
--------------------------------------------------------------------------------

-- 64 bit code layout, from least to most significant bits:
--   4   bits  : opcode      (positive)
--   8   bits  : operand arg (positive)
--   52  bits  : offset  arg (possibly negative)

type Op = Int64

pack :: Int64 -> Int -> Word8 -> Op
pack op offset arg =
      op
  .|. unsafeShiftL (fromIntegral arg) 4
  .|. unsafeShiftL (fromIntegral offset) 12

unpack1 :: Op -> Int64
unpack1 op = op .&. 15

unpack2 :: Op -> (Int64, Int)
unpack2 op = (op .&. 15, fromIntegral (unsafeShiftR op 12))

unpack3 :: Op -> (Int64, Int, Word8)
unpack3 op = (
  op .&. 15,
  fromIntegral (unsafeShiftR op 12),
  fromIntegral (unsafeShiftR op 4 .&. 255))

pattern Add o a    <- (unpack3 -> (0 , o, a)) where Add o a    = pack 0 o a
pattern Mov o      <- (unpack2 -> (1 , o)   ) where Mov o      = pack 1 o 0
pattern Assign o a <- (unpack3 -> (2 , o, a)) where Assign o a = pack 2 o a
pattern JZ o       <- (unpack2 -> (3 , o)   ) where JZ o       = pack 3 o 0
pattern LOOP o     <- (unpack2 -> (4 , o)   ) where LOOP o     = pack 4 o 0
pattern Get o      <- (unpack2 -> (5 , o)   ) where Get o      = pack 5 o 0
pattern Put o      <- (unpack2 -> (6, o)    ) where Put o      = pack 6 o 0
pattern Halt       <- (unpack1 -> 7         ) where Halt       = pack 7 0 0

showOp :: Op -> String
showOp = \case
  Add o a    -> "Add " ++ show o ++ " " ++ show a
  Mov o      -> "Mov " ++ show o
  Assign o a -> "Assign " ++ show o ++ " " ++ show a
  JZ o       -> "JZ " ++ show o
  LOOP o     -> "LOOP " ++ show o
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

instance NFData Block where
  rnf (Basic ops) = rnf ops
  rnf (While bs)  = rnf bs

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

pBlocks :: P.Parser [Block]
pBlocks = P.many' $
      Basic <$> pBasic
  <|> While <$> (P.char '[' *> pBlocks <* P.char ']')

parse :: B.ByteString -> [Block]
parse = either (error "Parse error") id
      . P.parseOnly (pBlocks <* P.endOfInput)
      . B.filter (P.inClass "+-<>,.[]")

-- Optimization
--------------------------------------------------------------------------------

optBlock :: [Op] -> [Op]
optBlock = merge . offsets 0 where

  offsets o = \case
    Mov o'     : ops -> offsets (o + o') ops
    Add _ a    : ops -> Add o a    : offsets o ops
    Get _      : ops -> Get o      : offsets o ops
    Put _      : ops -> Put o      : offsets o ops
    Assign _ v : ops -> Assign o v : offsets o ops
    other      : ops -> other      : offsets o ops
    []               -> [Mov o | o /= 0]

  merge = \case
    Add _ 0 : ops                           -> merge ops
    Add o1 a    : Add o2 b : ops | o1 == o2 -> merge (Add o1 (a + b) : ops)
    Assign o1 a : Add o2 b : ops | o1 == o2 -> merge (Assign o1 (a + b) : ops)
    Add o1 a : Assign o2 b : ops | o1 == o2 -> merge (Assign o2 b : ops)
    other                  : ops            -> other : merge ops
    []                                      -> []

opt :: [Block] -> [Block]
opt = map $ \case
  Basic b  -> Basic $ optBlock b
  While bs -> While $ opt bs

-- Code generation
--------------------------------------------------------------------------------

linearize :: [Block] -> [Op]
linearize blocks = evalState (go blocks) 0 [Halt] where

  go :: [Block] -> State Int ([Op] -> [Op])

  go (Basic ops:bs) = do
    modify (+(fromIntegral $ length ops))
    ((ops++).) <$> go bs

  go (While bs:bss) = do
    open  <- get <* modify (+1)
    bs'   <- go bs
    close <- get <* modify (+1)
    (((JZ (close + 1):) . bs' . (LOOP open:)).) <$> go bss

  go [] = pure id


-- Compilation
--------------------------------------------------------------------------------

type Code = V.Vector Int64

compile :: B.ByteString -> Code
compile = V.fromList . linearize . opt . parse

-- Interpretation
--------------------------------------------------------------------------------

memSize :: Int
memSize = 30000

_index  = V.unsafeIndex
_modify = MV.unsafeModify
_read   = MV.unsafeRead
_write  = MV.unsafeWrite

-- TODO : use ByteArray instead of Vector
run :: Code -> IO ()
run code = do
  !(dat :: MV.IOVector Word8) <- MV.replicate memSize 0

  let go :: Int -> Int -> IO ()
      go !ip !i = do
        case _index code ip of
          Add o a -> do
            _modify dat (+a) (i + o)
            go (ip + 1) i
          Mov o -> do
            go (ip + 1) (i + o)
          Assign o a -> do
            _write dat (i + o) a
            go (ip + 1) i
          JZ o -> do
            _read dat i >>= \case
              0 -> go o i
              _ -> go (ip + 1) i
          LOOP o -> do
            _read dat i >>= \case
              0 -> go (ip + 1) i
              _ -> go (o  + 1) i
          Get o -> do
            c <- getChar
            _write dat (i + o) (fromIntegral (ord c))
            go (ip + 1) i
          Put o -> do
            b <- _read dat (i + o)
            putChar (chr (fromIntegral b))
            go (ip + 1) i
          Halt -> do
            pure ()
          _ -> error "run: invalid opcode"

  go 0 0

