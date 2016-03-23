{-# language
  OverloadedStrings, LambdaCase, BangPatterns,
  PatternSynonyms, ViewPatterns, UnboxedTuples,
  ScopedTypeVariables, NoMonomorphismRestriction #-}

-- {-# options_ghc -Wall #-}

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

import Debug.Trace
import Control.Concurrent


hello = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

squares = "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"

hello2 = ">++++++++[-<+++++++++>]<.>>+>-[+]++>++>+++[>[->+++<<+++>]<<]>-----.>->+++..+++.>-.<<+[>[+>+]>>]<--------------.>>.+++.------.--------.>+.>+."

cellSize = "++++++++[>++++++++<-]>[<++++>-]+<[>-<[>++++<-]>[<++++++++>-]<[>++++++++<-]+>[>++++++++++[>+++++<-]>+.-.[-]<<[-]<->]<[>>+++++++[>+++++++<-]>.+++++.[-]<<<-]]>[>++++++++[>+++++++<-]>.[-]<<-]<+++++++++++[>+++>+++++++++>+++++++++>+<<<<-]>-.>-.+++++++.+++++++++++.<.>>.++.+++++++..<-.>>-[[-]<]"


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

pattern Add o a    <- (unpack3 -> (0 , o, a)) where Add o a    = pack 0  o a
pattern Mov o      <- (unpack2 -> (1 , o)   ) where Mov o      = pack 1  o 0
pattern Assign o a <- (unpack3 -> (2 , o, a)) where Assign o a = pack 2  o a
pattern AddMul o a <- (unpack3 -> (3 , o, a)) where AddMul o a = pack 3  o a
pattern SetTmp     <- (unpack1 -> 4         ) where SetTmp     = pack 4  0 0
pattern AddTmp o   <- (unpack2 -> (5 , o)   ) where AddTmp o   = pack 5  o 0
pattern SubTmp o   <- (unpack2 -> (6 , o)   ) where SubTmp o   = pack 6  o 0
pattern JZ o       <- (unpack2 -> (7 , o)   ) where JZ o       = pack 7  o 0
pattern JMP o      <- (unpack2 -> (8 , o)   ) where JMP o      = pack 8  o 0
pattern Get o      <- (unpack2 -> (9 , o)   ) where Get o      = pack 9  o 0
pattern Put o      <- (unpack2 -> (10, o)   ) where Put o      = pack 10 o 0
pattern Halt       <- (unpack1 -> 11        ) where Halt       = pack 11 0 0

showOp :: Op -> String
showOp = \case
  Add o a    -> "Add " ++ show o ++ " " ++ show a   
  Mov o      -> "Mov " ++ show o
  Assign o a -> "Assign " ++ show o ++ " " ++ show a
  AddMul o a -> "AddMul " ++ show o ++ " " ++ show a
  SetTmp     -> "SetTmp"
  AddTmp o   -> "AddTmp " ++ show o
  SubTmp o   -> "SubTmp " ++ show o
  JZ o       -> "JZ " ++ show o
  JMP o      -> "JMP " ++ show o
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

loopElim :: [Block] -> Maybe [Op]
loopElim blocks = do
  ops <- case blocks of
    [Basic ops] -> pure ops
    _           -> empty
  
  let eliminable = \case
        Add{}    -> True
        Mov{}    -> True
        _        -> False

  guard $ sum [o | Mov o <- ops] == 0
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
    While bs -> [While $ opt bs]
    -- While bs -> [maybe (While bs') Basic (loopElim bs')]
    --   where bs' = opt bs

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
    (((JZ (close + 1):) . bs' . (JMP open:)).) <$> go bss
    
  go [] = pure id


-- Compilation
--------------------------------------------------------------------------------

type Code = V.Vector Int64

compile :: B.ByteString -> Code
compile = V.fromList . linearize . opt . parse

noOptCompile :: B.ByteString -> Code
noOptCompile = V.fromList . linearize . parse

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

  let go :: Int -> Int -> Word8 -> IO ()
      go !ip !i !tmp = do
        case _index code ip of
          Add o a -> do
            _modify dat (+a) (i + o)
            go (ip + 1) i tmp
          Mov o -> do
            go (ip + 1) (i + o) tmp
          Assign o a -> do
            _write dat (i + o) a
            go (ip + 1) i tmp
          AddMul o a -> do
            _modify dat (\x -> x + a * tmp) (i + o)
            go (ip + 1) i tmp
          SetTmp -> do
            go (ip + 1) i =<< _read dat i
          AddTmp o -> do
            _modify dat (+tmp) (i + o)
            go (ip + 1) i tmp
          SubTmp o -> do
            _modify dat (subtract tmp) (i + o)
            go (ip + 1) i tmp
          JZ o -> do
            _read dat i >>= \case
              0 -> go o i tmp
              _ -> go (ip + 1) i tmp
          JMP o -> do
            go o i tmp
          Get o -> do
            c <- getChar
            _write dat (i + o) (fromIntegral (ord c))
            go (ip + 1) i tmp
          Put o -> do
            b <- _read dat (i + o)
            putChar (chr (fromIntegral b))
            go (ip + 1) i tmp
          Halt -> do
            pure ()
          _ -> error "run: invalid opcode"

  go 0 0 0 
  
