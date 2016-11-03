{-# LANGUAGE BangPatterns #-}

import Prelude
import qualified Data.Map as Map
import Text.Show.Functions
import Control.Monad.Error
import Control.Monad.State.Strict
import System.IO
import Control.Monad
import SyntaxTree
import Eval
import MiscUtil
import Parser
import System.Environment(getArgs)
import Data.List
import Data.IORef

{-
sampleFunction = Proc f ["a", "b"] where
    f = do
      a <- findVar "a"
      b <- findVar "b"
      case (a,b) of
        ((Value a), (Value b)) -> return (Value (a+b))
        _ -> throwError ("type error: NUMBER expected")
-}

isValue (Value _) = True
isValue _ = False

-- päivitä olemassaolevaa äläkä lisää uutta
setBang = BuiltInProc f where
    f ((Sym id):expr:[]) =
      eval expr >>= \x -> makeSymRef x >>= \z -> updateSym id z >> return x
    f _ = throwError "invalid set!"

success = Quoted (Sym "")

builtinDisplay = BuiltInProc f where
    f [x] = do
      x' <- eval x
      liftIO $ putStr (show x')
      return success
    f _ = throwError "display: one parameter expected"

builtinNewline = BuiltInProc f where
    f [] = liftIO (putStrLn "") >> return success
    f _ = throwError "newline: no parameters expected"


builtinArithm op null = BuiltInProc f where
    f args = do
      evaluated <- mapM' eval args
      if all isValue evaluated
      then return $ foldl' (\(Value x) (Value y) -> Value (x `op` y)) (Value (SNum null)) evaluated
      else throwError "type error: NUMBER expected"

plus' = BuiltInProc f where
    f args = do
      evaluated <- mapM' eval args
--      liftIO $ print evaluated
      if all isValue evaluated
      then return $ foldl' (\ !(Value x) !(Value y) -> Value (x + y)) (Value (SNum 0)) evaluated
      else throwError "type error: NUMBER expected"

minus' = BuiltInProc f where
    f args = do
      evaluated <- mapM' eval args
      if all isValue evaluated
      then return $ foldl1' (\(Value x) (Value y) -> Value (x - y)) evaluated
      else throwError "type error: NUMBER expected"

builtinArithm1 op = BuiltInProc f where
    f args = do
      evaluated <- mapM' eval args
      if all isValue evaluated
      then return $ foldl1' (\(Value x) (Value y) -> Value (x `op` y)) evaluated
      else throwError "type error: NUMBER expected"


builtinQuote = BuiltInProc (\[x] -> return x)

builtinPlus = builtinArithm (+) 0
builtinMinus = builtinArithm1 (-)
builtinTimes = builtinArithm (*) 1
builtinDiv = builtinArithm1 (/)

builtinArithUnaryOp op = BuiltInProc f where
    f [x] = do
      x' <- eval x
      case x' of
        Value a -> return $ op a
        _ -> throwError "expected numbers"
    f _ = throwError "wrong number of arguments in primitive arithmetic unary operator"

builtinArithBinOp op = BuiltInProc f where
    f [x,y] = do
      x' <- eval x
      y' <- eval y
      case (x',y') of
        (Value a, Value b) -> return $! a `op` b
        _ -> throwError "expected numbers"
    f _ = throwError "wrong number of arguments in primitive arithmetic binary operator"

builtinFloor = builtinArithUnaryOp $ \a -> Value (SNum $ floor (sNumToReal a))

builtinApply = BuiltInProc f where
    f (x:xs:[]) = do
      x' <- eval x
      xs' <- eval xs
      let xs'' = case unquoted xs' of
               (Sequence x) -> x

      apply x' xs''
    f _ = throwError "apply: procedure expected"

builtinEval = BuiltInProc f where
    f [x] = eval >=> (mangle . unquoted) >=> eval $ x
    f _ = throwError "eval: expected one argument"

builtinEquals = BuiltInProc f where
    f (a:b:[]) = do
        a' <- eval a
        b' <- eval b
        return $ Boolean (a' == b')
    f _ = throwError "=: wrong number of arguments"

modulo [x,y] = do
  x' <- eval x
  y' <- eval y
  case (x',y') of
    (Value (SNum a), Value (SNum b)) -> return $ Value (SNum (a `mod` b))
    _ -> throwError $ "modulo: invalid arguments -- " ++ show [x', y']
modulo x = throwError $ "modulo: invalid arguments -- " ++ show x

lazycons (x:y:[]) = do
  return $ case y of
             (Sequence s) -> Sequence (x:s)
             (Quoted (Sequence l)) -> Quoted $ Sequence (x:l)
             (Improper l e) -> Improper (x:l) e
             _ -> Improper [x] y
lazycons _ = throwError "cons: wrong number of arguments"

cons (x:y:[]) = do
  x' <- eval x
  y' <- eval y
  lazycons [x',y']

builtinCons = BuiltInProc cons

builtinMap = BuiltInProc f where
    f (func:lists) = do
      lists' <- mapM' eval lists
      func' <- eval func
      result <- g func' lists'
      return $ Sequence result
        where
          g _ [] = return []
          g _ l | any isNullList l = return []
          g f l = do
            firsts <- mapM' rCar l
            lasts <- mapM' rCdr l
            result <- apply f firsts
            rest <- g f lasts
            return $ result : rest
    f _ = error "map: invalid parameters"

builtinLength = BuiltInProc f where
    f [exp] = do
        x' <- eval exp
        seq <- case x' of
          (Quoted (Sequence xs)) -> return xs
          Sequence xs -> return xs
          _ -> throwError "length: not a list"
        return $ Value $ SNum (genericLength seq)
    f x = throwError $ "length: not a list -- " ++ (show x)

car (x:[]) = do
  x' <- eval x
  rCar x'
car x = throwError $ "car: too many arguments -- " ++ show x

builtinSetCarBang = BuiltInProc f where
    f [Sym s, expr] = do
      ref <- findVarRef s
      expr' <- eval expr
      x <- liftIO $ readIORef ref
      x' <- case x of
              (Quoted (Sequence (_:xs))) -> return (Sequence (expr':xs))
              (Sequence (_:xs)) -> return (Sequence (expr':xs))
              _ -> throwError "set-car!: invalid arguments"
      liftIO $ writeIORef ref x'
      return success
    f _ = throwError "set-car!: invalid arguments"

builtinSetCdrBang = BuiltInProc f where
    f [Sym s, expr] = do
      ref <- findVarRef s
      expr' <- eval expr
      l <- liftIO $ readIORef ref
      l' <- case l of
            (Quoted (Sequence (x:_))) -> lazycons [x,expr']
            (Sequence (x:_)) -> lazycons [x,expr']
            x -> throwError $ "set-cdr!: invalid arguments -- " ++ show x
      liftIO $ writeIORef ref l'
      return success
    f _ = throwError "set-cdr!: invalid arguments"
cdr (x:[]) = eval x >>= rCdr
cdr _ = throwError "cdr: invalid arguments"

builtinList = BuiltInProc f where
    f args = do
      args' <- mapM' eval args
      return $ Quoted (Sequence args')


builtinLoad = BuiltInProc f where
    f [Str s] = loadFile s
    f _ = throwError "could not read file"

loadFile s = do
  str <- liftIO $ readFile s
  exprs <- parseAll str
  results <- mapM' (mangle >=> eval) exprs
  return (Sequence results)

isEq = f where
    f [Sym x, Sym y] = do
      x' <- findVarRef x
      y' <- findVarRef y
      val1 <- liftIO $ readIORef x'
      val2 <- liftIO $ readIORef y'
      return $ Boolean $
             if x' == y'
               then True
               else
                   if isNullList val1 && isNullList val2 -- (eq? '() '())
                     then True
                     else
                         if not(isReferenceValue val1 || isReferenceValue val2)
                         then val1 == val2
                         else False

    f [x,y] = do
      x' <- eval x
      y' <- eval y
      return $ Boolean $ if isNullList x' && isNullList y'
         then True
         else case (x',y') of
                (Sym a, Sym b) -> a == b
                (Quoted (Sym a), Sym b) -> a == b
                (Sym a, Quoted (Sym b)) -> a == b
                (Value a, Value b) -> a == b
                (Quoted (Sym a), Quoted (Sym b)) -> a == b
                _ -> False -- x' == y'
    f _ = throwError "eq?: invalid number of arguments"

isEqual [x,y] = do
  x' <- eval x
  y' <- eval y
  return $ Boolean $ (show x') == (show y')
isEqual _ = throwError "equal?: invalid number of arguments"

builtinRelNumOp op = BuiltInProc f where
    f [x,y] = do
      x' <- eval x
      y' <- eval y
      return $ case (x',y') of
                 (Value a, Value b) -> Boolean (a `op` b)
                 _ -> Boolean False
    f _ = throwError "invalid number of arguments"

builtinAnd = BuiltInProc f where
    f xs = do
      xs' <- mapM' eval xs
      return $ Boolean (all (/=(Boolean False)) xs')

builtinOr = BuiltInProc f where
    f xs = do
      xs' <- mapM' eval xs
      return $ Boolean (not $ all (==(Boolean False)) xs')

builtinError = BuiltInProc f where
    f xs = do
      xs' <- mapM eval xs
      throwError $ "ERROR: " ++ (unwords $ (map show) xs')

builtinDelay = f where
    f [x] = do
      placeholder <- liftIO $ newIORef (Nothing :: Maybe AST)
      (env:_) <- get
      return $ Promise x env placeholder

builtinForce = f where
    f [Promise x env result] = do
                   val <- liftIO $ readIORef result
                   case val of
                     Just x -> return x
                     Nothing -> do
                              oldEnv <- get
                              put [env]
                              res <- eval x
                              liftIO $ writeIORef result (Just res)
                              put oldEnv
                              return res
    f [x] = eval x >>= f . return
    f _ = throwError "force: promise expected"

predicate p = BuiltInProc f where
    f [x] = do
      x' <- eval x
      return $ Boolean (p x')

run expr = do
  {result <- expr  (\e -> liftIO (putStrLn e) >> return ());
  liftIO $ putStrLn ("result: " ++ (show result));
  return () } `catchError` (\e -> liftIO $ putStrLn e >> return ())

initialEnv = Map.fromList $ [
                          ("+", plus'), --builtinPlus),
                          ("-", minus'), --builtinMinus),
                          ("*", builtinTimes),
                          ("/", builtinDiv),
                          ("=", builtinEquals),
                          ("<", builtinRelNumOp (<)),
                          (">", builtinRelNumOp (>)),
                          ("<=", builtinRelNumOp (<=)),
                          (">=", builtinRelNumOp (>=)),
                          ("or", builtinOr),
                          ("and", builtinAnd),
                          ("eq?", BuiltInProc isEq),
                          ("equal?", BuiltInProc isEqual),
                          ("set!", setBang),
                          ("set-car!", builtinSetCarBang),
                          ("set-cdr!", builtinSetCdrBang),
                          ("car", BuiltInProc car),
                          ("cdr", BuiltInProc cdr),
                          ("cons", builtinCons),
                          ("list", builtinList),
                          ("display", builtinDisplay),
                          ("newline", builtinNewline),
                          ("apply", builtinApply),
                          ("eval", builtinEval),
                          ("floor", builtinFloor),
                          ("modulo", BuiltInProc modulo),
                          ("error", builtinError),
                          ("number?", predicate isNumber),
                          ("string?", predicate isString),
                          ("symbol?", predicate isSymbol),
                          ("pair?", predicate isPair),
                          ("list?", predicate isList),
                          ("null?", predicate isNullList),
                          ("integer?", predicate isInteger),
                          ("rational?", predicate isRational),
                          ("real?", predicate isReal),
                          ("procedure?", predicate isProcedure),
                          ("map", builtinMap),
                          ("length", builtinLength),
                          ("delay", BuiltInProc builtinDelay),
                          ("force", BuiltInProc builtinForce),
                          ("read", builtinRead),
                          ("quote", builtinQuote),
                          ("load", builtinLoad)
                     ] ++ cadars

cadars =
    let
        powerset = filterM (const [True,False])
        cadrs = nub $ delete [] [ x | x <- powerset "adadadadad", length x <= 5]
    in
      [ ("c" ++ x ++ "r", primCudder x) | x <- cadrs ]

evaluateString str env = runErrorT (evalStateT (parseOne str >>= mangle >>= eval) env)
evaluateFile file env = runErrorT (evalStateT (loadFile file) env)
--evaluateFile file env = readFile file >>= (`evaluateString` env)

builtinRead = BuiltInProc f where
    f [Str x] = readString x
    f [] = liftIO getLine >>= readString
    f _ = throwError "read: invalid arguments"

readString :: String -> ASTComputation
readString str = do
  parseOne str >>= \x -> mangle (Quoted x)

refifyEnv env = do
  f (Map.toList env) >>= return . Map.fromList
      where
        f [] = return []
        f ((x,y):xs) = do
                  y' <- newIORef y
                  rest <- f xs
                  return $ (x,y') : rest

main = do
  hSetBuffering stdin LineBuffering
  initialEnv' <- refifyEnv initialEnv
  topEnv <- newFrame initialEnv'
  let env = pushEnv topEnv emptyEnvironment : []
  args <- getArgs
  evaluateFile "predef.scm" env `catchError` (\err -> print err >> return (Left ""))
  e <- case length args of
         0 -> runErrorT (evalStateT runInteractive env)
         otherwise -> if (head args) == "-e"
                      then evaluateString (args !! 1) env
                      else evaluateFile (head args) env

  case e of
    Left e -> putStrLn e
    Right _ -> return ()

runInteractive = do
  liftIO $ putStr ">> " >> hFlush stdout
  line <- liftIO $ getLine -- jos epäonnistuu, lopetetaan (lähinnä eof?)
  do {
    result <- parseOne line >>= mangle >>= eval ;
    liftIO (putStrLn (show result));
    runInteractive
  } `catchError` (\err -> (liftIO $ putStrLn err) >> runInteractive)
