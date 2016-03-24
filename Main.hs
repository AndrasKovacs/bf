{-# language
  OverloadedStrings, LambdaCase, BangPatterns,
  PatternSynonyms, ViewPatterns, UnboxedTuples,
  ScopedTypeVariables #-}

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

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  !inp <- getArgs >>= \case
    path:[] -> B.readFile path
    _       -> putStrLn "usage: fastbf PATH" >> exitSuccess
  run $ linearize $ opt $ parse inp

-- Opcodes
--------------------------------------------------------------------------------

-- 32 bit code layout, from least to most significant bits:
--   3   bits  : opcode      (positive)
--   8   bits  : operand arg (positive)
--   24  bits  : offset  arg (possibly negative)

type Op = Int32

pack :: Int32 -> Int -> Word8 -> Op
pack op offset arg =
      op
  .|. unsafeShiftL (fromIntegral arg) 3
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

pattern Add o a    <- (unpack3 -> (0 , o, a)) where Add o a    = pack 0 o a
pattern Assign o a <- (unpack3 -> (1 , o, a)) where Assign o a = pack 1 o a
pattern Get o      <- (unpack2 -> (2 , o)   ) where Get o      = pack 2 o 0
pattern Put o      <- (unpack2 -> (3, o)    ) where Put o      = pack 3 o 0

pattern Mov o      <- (unpack2 -> (4 , o)   ) where Mov o      = pack 4 o 0
pattern JZ o       <- (unpack2 -> (5 , o)   ) where JZ o       = pack 5 o 0
pattern JNZ o      <- (unpack2 -> (6 , o)   ) where JNZ o      = pack 6 o 0
pattern Halt       <- (unpack1 -> 7         ) where Halt       = pack 7 0 0

isCtrl :: Op -> Bool
isCtrl op = (op .&. 7) > 3

showOp :: Op -> String
showOp = \case
  Add o a    -> "Add " ++ show o ++ " " ++ show a
  Mov o      -> "Mov " ++ show o
  Assign o a -> "Assign " ++ show o ++ " " ++ show a
  JZ o       -> "JZ " ++ show o
  JNZ o      -> "JNZ " ++ show o
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
    (((JZ (close + 1):) . bs' . (JNZ (open + 1):)).) <$> go bss

  go [] = pure id

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
        !op <- peek ip
        if isCtrl op then
          case op of
            Mov o ->
              case (advancePtr ip 1, advancePtr i o) of
                (ip, i) ->
                  peek ip >>= \case
                    JZ o -> do
                      peek i >>= \case
                        0 -> go (advancePtr code o) i
                        _ -> go (advancePtr ip 1) i
                    JNZ o -> do
                      peek i >>= \case
                        0 -> go (advancePtr ip 1) i
                        _ -> go (advancePtr code o) i
                    Halt -> do
                      pure ()
            JZ o -> do
              peek i >>= \case
                0 -> go (advancePtr code o) i
                _ -> go (advancePtr ip 1) i
            JNZ o -> do
              peek i >>= \case
                0 -> go (advancePtr ip 1) i
                _ -> go (advancePtr code o) i
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

