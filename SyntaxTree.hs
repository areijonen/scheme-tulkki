{-# LANGUAGE BangPatterns #-}
module SyntaxTree(AST(
              Value, Boolean, Quoted, Define, Proc, BuiltInProc, Sequence, Block,
              Improper, Sym, Str, Promise, If, Macro),
                  SNumber(SNum, SFloat, SRat),
           CallStack, Env,
           emptyFrame, newFrame, insertSym, updateSym, findVar, findVarRef, makeSymRef,
           pushEnv, emptyEnvironment, sNumToReal, isNumber, isInteger, isRational, isReal,
           isString, isSymbol, isPair, isList, isNullList, isProcedure, isReferenceValue,
           SComputation, ASTComputation) where

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State.Strict
import Data.IORef
import Data.Ratio

-- toivotaan ettei monaditransformereilla pinoaminen tapa suorituskykyä..
type StateWithErrorIO s e a = StateT s (ErrorT e IO) a

-- kääritään AST tilamonaditransformeriin joka
-- pitää yllä Env-tilaa ja tehdään IO-toimintoja
-- joista sallitaan merkkijonomuotoiset poikkeukset
type SComputation a = StateWithErrorIO CallStack String a
type ASTComputation = SComputation AST

type FunArgs = [String]

-- oikeastaan pino
--data Env = Env (IORef (Map.Map String AST)) (Maybe (IORef Env))

type FrameRep = Map.Map String (IORef AST)

type Frame = IORef FrameRep

type Env = [Frame]

type CallStack = [Env]

emptyFrame :: IO Frame
emptyFrame = newIORef (Map.empty)

newFrame :: FrameRep -> IO Frame
newFrame = newIORef

emptyEnvironment = []

pushEnv :: Frame -> Env -> Env
pushEnv item env = item:env

topFrame [] = fail "topFrame failure"
topFrame (env:_) = return env

popFrame :: SComputation ()
popFrame =
    let
        pop [] = []
        pop (_:rest) = rest
    in modify pop

{-
pushFrame :: SComputation ()
pushFrame = do
  env <- get
  frame <- liftIO $ emptyFrame
  modify $ (frame:)
-}

lookupSymRef :: String -> Env -> IO (Maybe (IORef AST))
lookupSymRef sym = f
    where
      f [] = return Nothing
      f (env:rest) = do
        env' <- readIORef env
        case Map.lookup sym env' of
          Just x -> return $ Just x
          Nothing -> f rest


isReferenceValue (Value _) = False
isReferenceValue (Boolean _) = False
isReferenceValue (Sequence []) = False
isReferenceValue (Quoted (Sequence [])) = False
isReferenceValue _ = True

findVar :: String -> ASTComputation
findVar sym = do
  ref <- findVarRef sym
  liftIO $ readIORef ref

findVarRef :: String -> SComputation (IORef AST)
findVarRef sym = do
  env <- get
  res <- liftIO $ lookupSymRef sym (head env)
  case res of
    Just x -> return x
    Nothing -> throwError $ "Unbound variable: " ++ sym

insertSym :: String -> IORef AST -> SComputation ()
insertSym sym val = do
  env <- get
  top <- topFrame (head env)
  liftIO $ top `modifyIORef`(Map.insert sym val)
  put env

insertSymbols :: FrameRep -> [(String, IORef AST)] -> FrameRep
insertSymbols = foldr (uncurry Map.insert)

insertSymbolsM :: [(String,IORef AST)] -> Frame -> IO ()
insertSymbolsM syms env = env `modifyIORef` (`insertSymbols` syms)

makeSymRef :: AST -> SComputation (IORef AST)
makeSymRef x = liftIO (newIORef x)

modifySymRef x f= liftIO $ modifyIORef x f
-- set!
updateSym :: String -> IORef AST -> SComputation ()
updateSym sym val = get >>= liftIO . update . head
    where
      update [] = error ("Unbound variable: " ++ sym)
      update (frameRef:env) = do
          frame <- readIORef frameRef
          case Map.lookup sym frame of
            Just x -> frameRef `modifyIORef` (Map.insert sym val)
            Nothing -> update env


data AST = Value !SNumber
         | Boolean Bool
         | Quoted AST
         | Define String AST
         | Proc AST FunArgs Env
         | Macro AST FunArgs Env
         | BuiltInProc ([AST] -> ASTComputation) -- ottaa ast:n suoraan evaluoimattomana
	 | Sequence [AST]
         | Block [AST] -- (begin ...)
         | Improper [AST] AST -- Improper list, esim. (a . b)
         | Sym String
         | Str String
         | Promise !AST Env (IORef (Maybe AST))
         | If AST AST AST

instance Eq AST where
    (Value x) == (Value y) = x == y
    (Sequence xs) == (Sequence ys) = all (\(x,y) -> x == y) (zip xs ys)
    (Sym x) == (Sym y) = x == y
    (Quoted x) == (Quoted y) = x == y
    (Boolean x) == (Boolean y) = x == y
    _ == _ = False

data SNumber = SNum !Integer
             | SFloat !Double
             | SRat !(Ratio Integer)
              deriving Eq

instance Ord SNumber where
    a `compare` b = (sNumToReal a) `compare` (sNumToReal b)

sNumToReal (SNum a) = fromInteger a
sNumToReal (SRat a) = fromRational a
sNumToReal (SFloat a) = a

instance Show SNumber where
    show (SNum x) = show x
    show (SFloat x) = show x
    show (SRat x) = show x

instance Show AST where
    show (Value x) = show x
    show (Boolean x) = if x then "#t" else "#f"
    show (Quoted x@(Sequence _)) = show x
    show (Quoted (Quoted x)) = "'" ++ show x
    show (Quoted x) = show x
    show (Define x ast) = "(define " ++ x ++ " " ++ (show ast) ++ ")"
    show (Proc _ _ _) = "#<procedure>"
    show (BuiltInProc _) = "#<builtin procedure>"
    show (Sequence x) = "(" ++ (unwords $ map show x) ++ ")"
    show (Block x) = "(begin " ++ (show x) ++ ")"
    show (Improper x y) = "(" ++ (unwords $ map show x) ++ " . " ++ (show y) ++ ")"
    show (Promise _ _ _) = "<promise>"
    show (Sym s) = s
    show (Str s) = "\"" ++ s ++ "\""
    show (If p t f) = "(if " ++ (show p) ++ " " ++ show t ++ " " ++ show f ++ ")"
    show (Macro _ _ _) = "<macro>"



-- helpotetaan numeeristen operaatioiden toteuttamista
instance Num SNumber where
    fromInteger = SNum
    SNum a + SNum b = SNum (a+b)
    SNum a + SFloat b = SFloat ((fromInteger a)+b)
    SFloat a + SNum b = SFloat (a+(fromInteger b))
    SFloat a + SFloat b = SFloat (a+b)
    SRat a + SNum b = SRat (a + (b % 1))
    SRat a + SRat b = SRat (a+b)
    SRat a + SFloat b = SFloat ((fromRational a) + b)
    a + b = SFloat (sNumToReal a + sNumToReal b)
    SNum a * SNum b = SNum (a*b)
    SNum a * SFloat b = SFloat ( (fromInteger a) * b)
    SFloat a * SFloat b = SFloat (a*b)
    SRat a * SNum b = SRat (a * (b % 1))
    SRat a * SRat b = SRat (a * b)
    a * b = SFloat (sNumToReal a * sNumToReal b)
    abs (SNum a) = SNum (abs a)
    abs (SFloat a) = SFloat (abs a)
    abs (SRat a) = SRat (abs a)
    signum = undefined
    negate (SNum a) = SNum (negate a)
    negate (SFloat a) = SFloat (negate a)
    negate (SRat a) = SRat (negate a)

instance Fractional SNumber where
    fromRational x = SRat x
    (SNum a) / (SNum b) = SRat (a%b)
    (SNum a) / (SRat b) = SRat $ (fromIntegral a) / b
    (SNum a) / (SFloat b) = SFloat $ (fromInteger a) / b
    (SRat a) / (SRat b) = SRat $ a / b
    (SRat a) / (SNum b) = SRat $ a / (fromIntegral b)
    (SRat a) / (SFloat b) = SFloat $ (fromRational a) / b
    (SFloat a) / (SNum b) = SFloat $ a / (fromIntegral b)
    (SFloat a) / (SRat b) = SFloat $ a / (fromRational b)
    (SFloat a) / (SFloat b) = SFloat (a / b)

isSym (Sym _) = True
isSym _ = False

isNumber (Value _) = True
isNumber _ = False

isInteger (Value (SNum _)) = True
isInteger _ = False

isRational (Value (SRat _)) = True
isRational _ = False

isReal (Value (SFloat _)) = True
isReal _ = False

isString (Str _) = True
isString _ = False

--isSymbol (Quoted (Str _)) = True
--isSymbol (Quoted (Sym _)) = True
isSymbol (Sym _) = True
isSymbol _ = False

isQuoted (Quoted x) = True
isQuoted _ = False

unQuote (Quoted x) = x
unQuote x = x

isPair (Sequence (_:_)) = True
isPair (Improper _ _) = True
isPair (Quoted (Sequence (_:_))) = True
isPair (Quoted (Improper _ _)) = True
isPair _ = False

isList (Sequence _) = True
isList (Quoted (Sequence _)) = True
isList _ = False

isNullList (Sequence []) = True
isNullList (Quoted (Sequence [])) = True
isNullList _ = False

isProcedure (BuiltInProc _) = True
isProcedure (Proc _ _ _) = True
isProcedure _ = False

unSym (Sym x) = x

unSequence (Sequence x) = x

