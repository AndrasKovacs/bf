{-# language OverloadedStrings, LambdaCase, BangPatterns, PatternSynonyms #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Addr
import Data.Primitive.Types

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.DeepSeq

import Data.Int
import Data.Bits

import System.Environment
import System.FilePath
import System.Directory


import Data.Function


import Criterion.Main

main = do
  !inp <- B.readFile  "mandelbrot.bf"
  defaultMain [bench "opt" $ whnf (V.fromList . map encode . linearize . opt . parse) inp]

-- main = do
--   !inp <- B.readFile  "mandelbrot.bf"
--   mapM_ print $ linearize $ opt $ parse $ inp


-- AST
--------------------------------------------------------------------------------

data Block
  = Basic [Op]
  | While [Block]
  deriving (Show)

data Op
  = Add !Int !Int    -- offset, operand
  | Mov !Int         -- operand
  | Assign !Int !Int -- offset, value
  | AddMul !Int !Int -- offset, multiplier
  | SetTmp
  | AddTmp !Int      -- offset
  | SubTmp !Int      -- offset
  | JZ  !Int         -- jump if zero
  | JMP !Int         -- unconditional jump
  | Get !Int         -- offset
  | Put !Int         -- offset    
  | Halt             
  deriving (Show)

instance NFData Block where
  rnf (Basic ops) = rnf ops
  rnf (While bs)  = rnf bs

instance  NFData Op where
  rnf Add{}    = ()
  rnf Mov{}    = ()
  rnf Get{}    = ()
  rnf Put{}    = ()
  rnf Assign{} = ()
  rnf AddMul{} = ()
  rnf SetTmp   = ()
  rnf AddTmp{} = ()
  rnf SubTmp{} = ()
  rnf JZ{}     = ()
  rnf JMP{}    = ()
  rnf Halt     = ()

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
optBlock = go 0 where
  go !o = \case
    -- update offset
    Mov o' : ops               -> go (o + o') ops
    
    -- rewrite
    Add _ 0 : ops              -> go o ops
    Add _ a : Add _ b : ops    -> go o (Add o (a + b) : ops)
    Assign o v : Add _ a : ops -> go o (Assign o (v + a) : ops)
    Add _ a : Assign o v : ops -> go o (Assign o v : ops)

    -- set offsets
    Add _ a    : ops -> Add o a    : go o ops
    Get _      : ops -> Get o      : go o ops
    Put _      : ops -> Put o      : go o ops
    Assign _ v : ops -> Assign o v : go o ops
    AddMul _ i : ops -> AddMul o i : go o ops
    other      : ops -> other      : go o ops

    -- perform Mov
    [] -> [Mov o | o /= 0]

-- | Assumption: input is already optimized
loopElim :: [Block] -> Maybe [Op]
loopElim blocks = do
  ops <- case blocks of
    [Basic ops] -> pure ops
    _           -> empty
  
  let eliminable = \case
        Add{}    -> True
        Mov{}    -> True
        Assign{} -> True
        _        -> False

  guard $ case last ops of Mov{} -> False; _ -> True
  guard $ all eliminable ops
  guard $ sum [a | Add 0 a <- ops] == (-1)

  let tmpIntro = \case
        Add 0 (-1) -> Assign 0 0
        Add o (-1) -> SubTmp o
        Add o 1    -> AddTmp o
        Add o n    -> AddMul o n
        other      -> other

  pure $ SetTmp : map tmpIntro ops    

opt :: [Block] -> [Block]
opt blocks = 
  blocks >>= \case
    Basic b  -> [Basic $ optBlock b]
    While bs -> [maybe (While bs') Basic (loopElim bs')]
      where bs' = opt bs

-- Bytecode
--------------------------------------------------------------------------------

-- 64 bit code layout, least significant bits first:
--   4 bit  : opcode      (positive)
--   30 bit : address arg (positive)
--   30 bit : operand arg (possibly negative)

pack :: Int64 -> Int -> Int -> Int64
pack op arg1 arg2 =
      op
  .|. unsafeShiftL (fromIntegral arg1) 4
  .|. unsafeShiftL (fromIntegral arg2) 34

unpack0 :: Int64 -> Int64
unpack0 op = op .&. 15

unpack1 :: Int64 -> (Int64, Int64)
unpack1 op = (op .&. 15, unsafeShiftR op 4 .&. 1073741823)

unpack2 :: Int64 -> (Int64, Int64, Int64)
unpack2 op = (op .&. 15, unsafeShiftR op 4 .&. 1073741823, unsafeShiftR op 34)


-- pattern AddCode o a    = 0  :: Int64
-- pattern MovCode o      = 1  :: Int64
-- pattern AssignCode = 2  :: Int64
-- pattern AddMulCode = 3  :: Int64
-- pattern SetTmpCode = 4  :: Int64
-- pattern AddTmpCode = 5  :: Int64
-- pattern SubTmpCode = 6  :: Int64
-- pattern JZCode     = 7  :: Int64
-- pattern JMPCode    = 8  :: Int64
-- pattern GetCode    = 9  :: Int64
-- pattern PutCode    = 10 :: Int64
-- pattern HaltCode   = 11 :: Int64

addCode    = 0  :: Int64
movCode    = 1  :: Int64
assignCode = 2  :: Int64
addMulCode = 3  :: Int64
setTmpCode = 4  :: Int64
addTmpCode = 5  :: Int64
subTmpCode = 6  :: Int64
jZCode     = 7  :: Int64
jMPCode    = 8  :: Int64
getCode    = 9  :: Int64
putCode    = 10 :: Int64
haltCode   = 11 :: Int64

encode :: Op -> Int64
encode = \case
  Add o a    -> pack addCode    o a
  Mov o      -> pack movCode    o 0
  Assign o a -> pack assignCode o a
  AddMul o a -> pack addMulCode o a
  SetTmp     -> pack setTmpCode 0 0
  AddTmp o   -> pack addTmpCode o 0
  SubTmp o   -> pack subTmpCode o 0
  JZ o       -> pack jZCode     o 0
  JMP o      -> pack jMPCode    o 0
  Get o      -> pack getCode    o 0
  Put o      -> pack putCode    o 0
  Halt       -> pack haltCode   0 0

linearize :: [Block] -> [Op]
linearize bs = evalState (go bs) 0 [Halt] where  
  go :: [Block] -> State Int ([Op] -> [Op])
  go (Basic ops:bs) = do {modify (+(length ops)); ((ops++).) <$> go bs}
  go (While bs:bss) = do
    open  <- get <* modify (+1)
    bs'   <- go bs
    close <- get <* modify (+1)
    (((JZ (close + 1):) . bs' . (JMP open:)).) <$> go bss
  go [] = pure id  

compile :: [Op] -> IO (MV.IOVector Int64)
compile = V.unsafeThaw . V.fromList . map encode


  

  
  







-- Pointer to pinned primitive data
-- type role Ptr nominal
-- newtype Ptr a = Ptr Addr

-- -- | Offset code
-- offset :: forall a. Prim a => Ptr a -> Int -> Ptr a
-- offset (Ptr addr) i = Ptr (plusAddr addr (i * I# (sizeOf# (undefined :: a))))
-- {-# inline offset #-}

-- -- | Read at an offset
-- readOffPtr :: (Prim a, PrimMonad m) => Ptr a -> Int -> m a
-- readOffPtr (Ptr addr) i = readOffAddr addr i
-- {-# inline readOffPtr #-}

-- writeOffPtr :: (Prim a, PrimMonad m) => Ptr a -> Int -> a -> m ()
-- writeOffPtr (Ptr addr) i a = writeOffAddr addr i a
-- {-# inline writeOffPtr #-}



-- genByteCode :: [Op] -> IO (Ptr Int)
-- genByteCode
    


-- load :: [Block] -> IO (Ptr Int)
-- load blocks = do
  




  
